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
;; Configure your offlineimaprc (or whatever you use to manage your maildir) to
;; sync that folder.
;;
;; This package works by associating message subjects with capture
;; templates. You should customise `org-capture-templates' to include actions
;; with the at least the following titles:
;;
;; * Todo
;; * Note
;; * Link
;; * Diary
;;
;; Furthermore, you should make an entry in `org-agenda-custom-commands' called
;; 'email' that should email a copy of your agenda to yourself.

;;; Usage:

;;
;; Once configured, you can send yourself an email and this package will perform
;; an appropriate action:
;;
;; * Messages whose body contains a link will be captured as a link.
;;
;; * Messages with 'diary' as the subject will be added to the diary.
;;
;; * Messages with 'agenda' as the subject will invoke a special 'Email'
;;   action. I have this configured to mail me a formatted copy of my agenda for
;;   the day.
;;
;; * In any other case, the message will be inserted at the appropriate tree for
;;   its capture template. If a capture template cannot be found, it will be
;;   inserted as a note using the 'Note' template.
;;
;; A message may contain special directives. They occur at the start of any line
;; and take the form <letter><space><args...>.
;;
;; * Schedule (s): The line is read as a time-stamp specification.
;;
;; * Deadline (d): The line is read as a time-stamp specification.
;;
;; * Tags (t): The line is interpreted as a space-separated list of tags.
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
(defun cbom:parse-subject (subj)
  "Map the SUBJECT of a message to a capture action."
  (cond
   ((s-blank? subj) "note")
   ((s-matches? "book" subj) "reading")
   ((s-matches? (rx (or "song" "music" "album")) subj)
    "listening")
   ((s-matches? (rx (or "diary" "calendar" "appt" "appointment")) subj)
    "diary")
   (t
    (s-downcase subj))))

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
  (let* (;; Make sure we're dealing with the plaintext message content.
         (body (if (cbom:multipart-message? msg)
                   (cbom:multipart-body-plaintext-section msg)
                 (cdr (cbom:split-message-head-and-body msg))))
         ;; Remove the pertinent lines from the plist.
         (lns (->> (s-lines body) (-map 's-trim) (-remove 's-blank?))))
    (list
     :filepath path
     :url (cbom:find-url body)
     :title
     (->> lns
       (--remove (s-matches? (rx bol (or "s" "d" "t") (+ space)) it))
       (s-join "\n"))
     :scheduled
     (->> lns
       (--keep (s-match (rx bol "s" (+ space) (group (* nonl))) it))
       (-map 'cadr)
       (car))
     :deadline
     (->> lns
       (--keep (s-match (rx bol "d" (+ space) (group (* nonl))) it))
       (-map 'cadr)
       (car))
     :tags
     (->> lns
       (--keep (s-match (rx bol "t" (+ space) (group (* nonl))) it))
       (-mapcat (-compose 's-split-words 'cadr))
       (-distinct))
     :kind
     (cbom:parse-subject (cbom:message-header-value "subject" msg)))))

;;; Org capture

;; IO [String]
(defun cbom:capture-keywords ()
  (-map (-compose 's-downcase 'cadr) org-capture-templates))

;; MessagePlist -> IO Bool
(cl-defun cbom:capture-candidate? (&key kind &allow-other-keys)
  (-contains? (cbom:capture-keywords) (s-downcase kind)))

;; MessagePlist -> IO ()
(cl-defun cbom:growl-notify (&key kind title &allow-other-keys)
  (let ((icon (f-join user-emacs-directory "assets" "org_unicorn.png")))
    (if (s-matches? "agenda" kind)
        (growl "Agenda Emailed" "" icon)
      (growl (format "%s Captured" (s-capitalize kind)) title icon))))

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

;; MessagePlist -> String
(cl-defun cbom:format-for-insertion
    (&key kind url title scheduled deadline &allow-other-keys)
  "Format a parsed message according to its kind."
  (cond

   ;; Give precedence to URLs.
   (url
    (format "[[%s][%s]]" url
            (or (ignore-errors (cbom:fetch-html-title url))
                url)))

   ;; Special diary format
   ((s-matches? "diary" kind)
    (format "%s\n<%s>" title (org-read-date nil nil scheduled)))

   ;; All other types can follow a standard style.
   (t
    (concat
     (if (s-matches? "todo" kind) (concat "TODO " title) title)
     (when scheduled (format "\nSCHEDULED: <%s>" (org-read-date nil nil scheduled)))
     (when deadline (format "\nDEADLINE: <%s>" (org-read-date nil nil deadline)))))))

;; String -> IO ()
(defun cbom:goto-capture-site (kind)
  "Move to the insertion site for the capture template associated with KIND."
  (cl-destructuring-bind (&optional key &rest rest_)
      (--first (equal (s-downcase (elt it 1)) (s-downcase kind))
               org-capture-templates)
    (org-capture-goto-target (or key "n"))
    (end-of-line)))

;; IO ()
(defun cbom:dispatch-agenda-email ()
  (let ((inhibit-redisplay t))
    (-when-let
        (key (car (--first
                   (and (listp (cdr it)) ; ignore improper lists.
                        (s-matches? "email" (cadr it)))
                   org-agenda-custom-commands)))
      (org-agenda nil key))))

;; MessagePlist -> IO ()
(defun cbom:capture (msg-plist)
  "Read MSG-PLIST and execute the appropriate capture behaviour."
  (cond
   ((s-matches? "agenda" (plist-get msg-plist :kind))
    (cbom:dispatch-agenda-email))
   (t
    (cbom:goto-capture-site (plist-get msg-plist :kind))
    (org-insert-subheading '(16))
    (insert (apply 'cbom:format-for-insertion msg-plist))
    (org-set-tags-to (plist-get msg-plist :tags))
    (org-set-property
     "CAPTURED"
     (s-with-temp-buffer
       (org-insert-time-stamp (current-time) t 'inactive))))))

;; MessagePlist -> IO ()
(cl-defun cbom:remove-message (&key filepath &allow-other-keys)
  ;; Create filepath to the destination dir, with filename tags that mark
  ;; the message as read.
  (let* ((dest-file (format "%s:2,S" (car (s-split ":" (f-filename filepath)))))
         (dest-filepath (f-join (cbom:org-processed-mail-folder) "cur" dest-file)))
    ;; Move to trash directory, with updated filename to tag as read.
    (f-move filepath dest-filepath)))

;; IO ()
(defun cbom:capture-messages ()
  "Parse and capture unread messages in `cbom:org-mail-folder'.
Captures messages subjects match one of the values in `org-capture-templates'.
Captured messages are marked as read."
  (interactive)
  (save-window-excursion
    (--each (-map 'cbom:parse-message
                  (cbom:unprocessed-messages (cbom:org-mail-folder)))
      (save-excursion
        (atomic-change-group
          (cbom:capture it)
          (save-buffer)
          (apply 'cbom:growl-notify it)
          (apply 'cbom:remove-message it))))))

;;; Timer

(hook-fn 'after-init-hook
  (defvar cbom:capture-timer
    (run-with-timer 5 10 (lambda ()
                           (when (featurep 'org)
                             (with-demoted-errors
                               (cbom:capture-messages)))))))

(provide 'cb-org-email-capture)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org-email-capture.el ends here
