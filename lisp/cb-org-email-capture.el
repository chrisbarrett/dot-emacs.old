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

;; Capture tasks from emails in maildir. Set `cbom:target-folder' to the
;; path of a maildir folder to search for capturable tasks.
;;
;; I have a server-side rule set up that moves any messages from my own address
;; to a folder called 'org'. This is the default maildir folder that will be searched.
;;
;; This package works by associating message subjects with capture
;; templates. You should customise `org-capture-templates' to include actions
;; with the following titles:
;;
;; * Todo
;; * Note
;; * Link
;; * Email
;; * Diary
;;
;; Once configured, you can send yourself an email and this package will perform
;; an action:
;;
;; * Messages with the 'todo' or 'link' subjects will have the message body
;;   added at the appropriate capture destination.
;;
;; * Messages whose body contains a link will be captured as a link.
;;
;; * Messages with 'diary' as the subject will be added to the diary. The first line
;;   in the message body is read by the org date parser, so you can write dates like
;;   'mon', '+3d', 'aug 10', etc. The next line is used for the entry's title.
;;
;; * Messages with 'agenda' as the subject will invoke a special 'Email'
;;   action. I have this configured to mail me a formatted copy of my agenda for
;;   the day.
;;
;; * In any other case, the message will be inserted at the appropriate tree for
;;   its capture template. If a capture template cannot be found, it will be inserted
;;   as a note using the 'Note' template.
;;

;;; Code:

(require 'cb-lib)
(require 'cb-paths)
(require 'async)
(autoload 'org-capture-goto-target "org-capture")
(autoload 'org-insert-link "org")
(autoload 'org-insert-subheading "org")
(autoload 'org-insert-todo-subheading "org")
(autoload 'org-read-date "org")

;;; Message reading
;;;
;;; The search path is memoised and should be accessed using the accessor function.

;; String
(defvar cbom:target-folder nil
  "Folder path to search for messages to use for capturing.")

;; IO String
(defun cbom:target-folder ()
  "Accessor for the variable of the same name.
Return the path to the 'org' folder in the maildir and memoise."
  (or cbom:target-folder
      (let ((dir (->> (f-join user-home-directory "Maildir")
                   (f-directories)
                   (-mapcat 'f-directories)
                   (--first (s-ends-with? "org" it)))))
        (setq cbom:target-folder dir)
        dir)))

;; [FilePath] -> IO [(String, FilePath)]
(defun cbom:unprocessed-messages (dir)
  "The unprocessed mail in DIR.
DIR should be an IMAP maildir folder containing a subdir called 'new'."
  (let ((new (f-join dir "new")))
    (when (f-exists? new)
      (->> (f-files new)
        (-map (-juxt 'f-read-text 'identity))
        ;; Remove messages dispatched by org functions, like the agenda.
        (--remove (s-starts-with? "[org]" (car it)))))))

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
  ;; Parse the body. We look at the body to determine if it's just a link.
  (-if-let* ((body (if (cbom:multipart-message? msg)
                       (cbom:multipart-body-plaintext-section msg)
                     (cdr (cbom:split-message-head-and-body msg))))
             (url (cbom:find-url body)))
      ;; If the body contains a url, capture that.
      (list :filepath path
            :body url
            :subject "link")
    ;; Otherwise parse the subject.
    (list :filepath path
          :body (s-trim body)
          :subject (cbom:fuzzy-parse-subject
                    (cbom:message-header-value "subject" msg)))))

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
  (cl-destructuring-bind (date title) (s-lines str)
    (format "%s\n<%s>" title (org-read-date nil nil date))))

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
      (let ((heading (plist-get msg-plist :body))
            (subtree-append '(16))
            (type (plist-get msg-plist :subject)))
        (cond
         ;; Capture todos.
         ((s-matches? "todo" type)
          (org-insert-todo-subheading subtree-append)
          (insert heading))
         ;; Capture links.
         ;; Assume the heading is a well-formed link.
         ((s-matches? "link" type)
          (org-insert-subheading subtree-append)
          (org-insert-link nil heading
                           (or (ignore-errors (cbom:fetch-html-title heading))
                               heading)))
         ;; Capture diary entries.
         ((s-matches? (rx (or "diary" "calendar" "appt" "appointment")) type)
          (org-insert-subheading subtree-append)
          (insert (cbom:format-diary-entry heading)))

         ;; Otherwise insert the plain heading.
         (t
          (org-insert-subheading subtree-append)
          (insert heading)))))))

;; IO ()
(defun cbom:dispatch-agenda-email ()
  ""
  (let ((inhibit-redisplay t))
    (-when-let (key (car (--first (s-matches? "email" (cadr it))
                                  org-capture-templates)))
      (org-capture nil key))))

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
(defun cbom:mark-as-read (msg-plist)
  "Move an unread message into the corresponding cur directory."
  (let* ((new (plist-get msg-plist :filepath))
         (file (f-filename (plist-get msg-plist :filepath)))
         (cur (f-join (f-parent (f-dirname new)) "cur"))
         ;; Appending tags to the filename is how the maildir format signifies
         ;; read messages.  See
         ;; <http://blog.steve.org.uk/delivering_to_a_maildir_folder__but_marking_as_read.html>
         (dest (format "%s:2,S" (f-join cur file))))
    ;; Move to read messages directory, with updated filename to tag as read.
    (f-move new dest)))

;; IO ()
(defun cbom:capture-messages ()
  "Parse and capture unread messages in `cbom:target-folder'.
Captures messages subjects match one of the values in `org-capture-templates'.
Captured messages are marked as read."
  (interactive)
  (save-window-excursion
    (save-excursion
      (--each (-map 'cbom:parse-message
                    (cbom:unprocessed-messages (cbom:target-folder)))
        (atomic-change-group
          (cbom:capture it)
          (cbom:growl-notify it)
          (cbom:mark-as-read it))))))

;;; Timer
;;;
;;; Run after init to ensure it does not interfere with the agenda startup.

(hook-fn 'after-init-hook
  (defvar cbom:capture-timer
    (run-with-timer 5 10 'cbom:capture-messages)))

(provide 'cb-org-email-capture)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org-email-capture.el ends here
