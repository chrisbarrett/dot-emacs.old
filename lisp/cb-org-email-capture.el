;;; cb-org-email-capture.el --- Capture tasks from emails in maildir

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130924.0129

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Capture emails in a specified maildir folder with org-mode.

;;; Configuration:

;; Create an IMAP folder called 'org' and create a server-side rule to move messages
;; from your own address into that folder.
;;
;; Processed messages are moved into your trash maildir directory. You can
;;
;; This package works by associating message subjects with capture
;; templates. You should customise `org-capture-templates' to include actions
;; with the following titles:
;;
;; * Todo
;; * Note
;; * Link
;; * Diary
;;
;; Furthermore, you may have an entry in `org-agenda-custom-commands' called
;; 'email' that should email a copy of your agenda to yourself.
;;
;; Once configured, you can send yourself an email and this package will perform
;; an appropriate action:
;;
;; * Messages whose body contains a link will be captured as a link.
;;
;; * Messages with the 'todo' or 'link' subjects will have the message body
;;   added at the appropriate capture destination.
;;
;; * Messages with 'diary' as the subject will be added to the diary.
;;
;; * Messages with 'agenda' as the subject will invoke a special 'Email'
;;   action. I have this configured to mail me a formatted copy of my agenda for
;;   the day.
;;
;; * In any other case, the message will be inserted at the appropriate tree for
;;   its capture template. If a capture template cannot be found, it will be inserted
;;   as a note using the 'Note' template.
;;
;; The body of a message may contain special directives.  To schedule an
;; item or set a deadline, start a line with the letter 's' or 'd', then a
;; space.  The remainder of the line is read as the timestamp.
;;
;; IMPORTANT: You should ensure that any messages in this maildir folder that
;; you do not want parsed and captured have subjects beginning with '[org]'
;; (without the quotes). This is to allow for other org commands to send messages
;; to your own address. For instance, my 'email' agenda command will send an email
;; to my own address and I don't want my agenda captured to a note!
;;

;;; Code:

(require 'cb-lib)
(require 'cb-paths)
(require 'async)
(autoload 'org-set-property "org")
(autoload 'org-insert-link "org")
(autoload 'org-insert-time-stamp "org")
(autoload 'org-insert-subheading "org")
(autoload 'org-insert-todo-subheading "org")
(autoload 'org-read-date "org")
(autoload 'org-capture-goto-target "org-capture")

;;; Customisable interface

;; FilePath
(defvar cbom:org-mail-folder nil
  "Folder path to search for messages to use for capturing.")

;; FilePath
(defvar cbom:org-processed-mail-folder nil
  "Folder path to move items once processed.")

;;; Internal

;;; Message reading
;;;
;;; The search path is memoised and should be accessed using the accessor function.

;; IO FilePath
(defun cbom:org-mail-folder ()
  "Accessor for the variable of the same name.
By default, return the path to the maildir 'org' folder and memoise."
  (or cbom:org-mail-folder
      (let ((dir (->> (f-join user-home-directory "Maildir")
                   (f-directories)
                   (-mapcat 'f-directories)
                   (--first (s-ends-with? "org" it)))))
        (setq cbom:org-mail-folder dir)
        dir)))

;; IO FilePath
(defun cbom:org-processed-mail-folder ()
  "Accessor for the variable of the same name.
By default, return the path to the maildir trash folder and memoise."
  (or cbom:org-processed-mail-folder
      (let ((dir (->> (f-join user-home-directory "Maildir")
                   (f-directories)
                   (-mapcat 'f-directories)
                   (--first (s-matches? (rx (or "trash" "deleted"))
                                        (car (last (s-split (f-path-separator) it))))))))
        (setq cbom:org-processed-mail-folder dir)
        dir)))

;; Maybe String -> Bool
(defun cbom:org-dispatched-message? (msg)
  "Test whether MSG has [org] in its subject."
  (-when-let (subj (cbom:message-header-value "subject" msg))
    (s-starts-with? "[org]" subj)))

;; [FilePath] -> IO [(String, FilePath)]
(defun cbom:unprocessed-messages (dir)
  "The unprocessed mail in DIR.
DIR should be an IMAP maildir folder containing a subdir called 'new'."
  (let ((new (f-join dir "new")))
    (when (f-exists? new)
      (->> (f-files new)
        (-map (-juxt 'f-read-text 'identity))
        ;; Remove messages dispatched by org functions, like the agenda.
        (-remove (-compose 'cbom:org-dispatched-message? 'car))))))

;;; Message processing

;; String -> String -> Maybe String
(defun cbom:message-header-value (header msg)
  (cadr (s-match (eval `(rx bol ,header ":" (* space) (group (* nonl)))) msg)))

;; String -> Bool
(defun cbom:multipart-message? (msg)
  (s-matches? "multipart/alternative" (cbom:message-header-value "content-type" msg)))

;; String -> (String, String)
(defun cbom:split-message-head-and-body (msg)
  (let ((div (s-index-of "\n\n" msg)))
    (cons (substring msg 0 div) (substring msg div))))

;; String -> String
(defun cbom:multipart-body-plaintext-section (msg)
  (cl-destructuring-bind (head . body)
      (cbom:split-message-head-and-body msg)
    (->> body
      ;; Split the body by the boundary specified in the header and select
      ;; the section with plaintext MIME encoding.
      (s-split (cadr (s-match (rx "boundary=" (group (* nonl))) head)))
      (--first (s-contains? "text/plain" it))
      ;; Tidy the section, removing MIME headers.
      (s-trim)
      (s-chop-suffix "--")
      (s-lines)
      (--drop-while (or (s-matches? (rx bol (or "charset" "content") (* nonl)
                                        (or "=" ":")) it)
                        (s-blank? it)))
      (s-join "\n")
      (s-trim)
      (s-chop-suffix "="))))

;; String -> String
(defun cbom:fuzzy-parse-subject (subject)
  "Map the SUBJECT of a message to a capture action."
  (cond
   ((s-matches? "link" subject) "link")
   ((s-matches? "todo" subject) "todo")
   ((s-matches? "book" subject) "reading")
   ((s-matches? "song" subject) "listening")
   ((s-matches? "music" subject) "listening")
   ((s-matches? "album" subject) "listening")
   (t
    subject)))

;; String -> Maybe String
(defun cbom:find-url (str)
  "Extract the first URL from STR. Performs loose matching."
  (car (s-match
        (rx bow
            (or
             ;; Match URLs, with and without protocol.
             (and "http" (? "s") "://")
             (and "www." (* alnum) ".")
             ;; Loosely match strings with common TLDs,
             (and (+ alnum) "." (or "edu" "net" "gov" "com" "biz" "org" "info" "co.")))
            (* (not (any space "\n" "\r"))))
        str)))

;; (String, FilePath) -> MessagePlist
(cl-defun cbom:parse-message ((msg path))
  "Parse message body, preferentially selecting links."
  (let ((body (if (cbom:multipart-message? msg)
                  (cbom:multipart-body-plaintext-section msg)
                (cdr (cbom:split-message-head-and-body msg)))))
    ;; We look at the body to determine if it's just a link.
    ;; If the body contains a url, capture this message as a link.
    (-if-let (url (cbom:find-url body))
        (list :filepath path
              :body url
              :subject "link")
      ;; Otherwise parse the subject to determine how to capture this message.
      ;; If no subject was provided, capture as a note.
      (list :filepath path
            :body (s-trim body)
            :subject
            (-if-let (subj (cbom:message-header-value "subject" msg))
                (cbom:fuzzy-parse-subject subj)
              "note")))))

;;; Org capture

;; IO [String]
(defun cbom:capture-keywords ()
  (-map (-compose 's-downcase 'cadr) org-capture-templates))

;; MessagePlist -> IO Bool
(defun cbom:capture-candidate? (msg-plist)
  (-contains? (cbom:capture-keywords)
              (s-downcase (plist-get msg-plist :subject))))

;; MessagePlist -> IO (String, String, ...)
(defun cbom:capture-template-for-plist (msg-plist)
  "Find an org capture template corresponding to the subject in MSG-PLIST."
  (--first
   (cl-destructuring-bind (_key title &rest rest_) it
     (equal (s-downcase title)
            (s-downcase (plist-get msg-plist :subject))))
   org-capture-templates))

;; MessagePlist -> IO ()
(defun cbom:growl-notify (msg-plist)
  (let ((type (plist-get msg-plist :subject))
        (icon (f-join user-emacs-directory "assets" "org_unicorn.png"))
        (body (plist-get msg-plist :body)))
    (cond
     ((s-matches? "agenda" type)
      (growl "Agenda Emailed" "" icon))
     (t
      (growl (format "%s Captured" (s-capitalize type)) body icon)))))

;; String -> IO String
(defun cbom:fetch-html-title (url)
  (with-current-buffer
      (url-retrieve-synchronously
       (if (s-matches? (rx "http" (? "s") "://") url)
           url
         (s-prepend "http://" url)))
    ;; Clear request status.
    (message nil)
    (cadr (s-match (rx "<title>" (group (* nonl)) "</title>")
                   (buffer-string)))))

;; String -> String
(defun cbom:format-diary-entry (str)
  (let* ((lns (->> (s-lines str) (-map 's-trim) (-remove 's-blank?)))
         (sched (->> lns
                  (--keep (s-match (rx bol "s" (+ space) (group (* nonl))) it))
                  (-map 'cadr)
                  (car)))
         (title (->> lns
                  (--remove (s-matches? (rx bol (or "s" "d") (+ space)) it))
                  (s-join "\n"))))
    (format "%s\n<%s>" title (org-read-date nil nil sched))))

;; String -> String
(defun cbom:format-body (str)
  (let* ((lns (->> (s-lines str) (-map 's-trim) (-remove 's-blank?)))
         (sched (->> lns
                  (--keep (s-match (rx bol "s" (+ space) (group (* nonl))) it))
                  (-map 'cadr)
                  (car)))
         (deadl (->> lns
                  (--keep (s-match (rx bol "d" (+ space) (group (* nonl))) it))
                  (-map 'cadr)
                  (car)))
         (title (->> lns
                  (--remove (s-matches? (rx bol (or "s" "d") (+ space)) it))
                  (s-join "\n"))))
    (concat
     title
     (when sched (format "\nSCHEDULED: <%s>" (org-read-date nil nil sched)))
     (when deadl (format "\nDEADLINE: <%s>" (org-read-date nil nil deadl))))))

;; MessagePlist -> IO ()
(defun cbom:capture-with-template (msg-plist)
  "Capture the data in MSG-PLIST into the destination in its
correspoding capture template."
  (cl-destructuring-bind (&optional key &rest rest_)
      (cbom:capture-template-for-plist msg-plist)
    (save-excursion
      ;; Capture as a note if a suitable template cannot be found.
      (org-capture-goto-target (or key "n"))
      (end-of-line)
      ;; Insert appropriate header, depending on capture type.
      (let ((msg (plist-get msg-plist :body))
            (subtree-append '(16))
            (type (plist-get msg-plist :subject)))
        (cond
         ;; Capture todos.
         ((s-matches? "todo" type)
          (org-insert-todo-subheading subtree-append)
          (insert (cbom:format-body msg)))
         ;; Capture links.
         ;; Assume the message body is a well-formed link.
         ((s-matches? "link" type)
          (org-insert-subheading subtree-append)
          (org-insert-link nil msg
                           (or (ignore-errors (cbom:fetch-html-title msg))
                               msg)))
         ;; Capture diary entries.
         ((s-matches? (rx (or "diary" "calendar" "appt" "appointment")) type)
          (org-insert-subheading subtree-append)
          (insert (cbom:format-diary-entry msg)))

         ;; Otherwise insert the plain heading.
         (t
          (org-insert-subheading subtree-append)
          (insert (cbom:format-body msg))))
        ;; Insert captured timestamp
        (org-set-property "CAPTURED" (s-with-temp-buffer
                                       (org-insert-time-stamp (current-time) t 'inactive)))))))

;; IO ()
(defun cbom:dispatch-agenda-email ()
  (let ((inhibit-redisplay t))
    (-when-let (key (car (--first (and (listp (cdr it)) ; ignore improper lists.
                                       (s-matches? "email" (cadr it)))
                                  org-agenda-custom-commands)))
      (org-agenda nil key))))

;; MessagePlist -> IO ()
(defun cbom:capture (msg-plist)
  "Read MSG-PLIST and execute the appropriate handler."
  (let ((subj (plist-get msg-plist :subject)))
    (cond
     ((s-matches? "agenda" subj)
      (cbom:dispatch-agenda-email))
     (t
      (cbom:capture-with-template msg-plist)))))

;; MessagePlist -> IO ()
(defun cbom:remove-message (msg-plist)
  "Move the message corresponding to MSG-PLIST to `cbom:org-processed-mail-folder'and mark as read."
  (let* ((new (plist-get msg-plist :filepath))
         (file (f-filename (plist-get msg-plist :filepath)))
         ;; Create filepath to trash cur dir, with filename tags that mark the
         ;; message as read.
         (dest-filename (format "%s:2,S" (car (s-split ":" file))))
         (dest (f-join (cbom:org-processed-mail-folder) "cur" dest-filename)))
    ;; Move to trash directory, with updated filename to tag as read.
    (f-move new dest)))

;; IO ()
(defun cbom:capture-messages ()
  "Parse and capture unread messages in `cbom:org-mail-folder'.
Captures messages subjects match one of the values in `org-capture-templates'.
Captured messages are marked as read."
  (interactive)
  (save-window-excursion
    (save-excursion
      (--each (-map 'cbom:parse-message
                    (cbom:unprocessed-messages (cbom:org-mail-folder)))
        (atomic-change-group
          (cbom:capture it)
          (cbom:growl-notify it)
          (cbom:remove-message it))))))

;;; Timer

(defvar cbom:capture-timer
  (run-with-timer 5 10 (lambda ()
                         (when (featurep 'org)
                           (cbom:capture-messages)))))

(provide 'cb-org-email-capture)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org-email-capture.el ends here
