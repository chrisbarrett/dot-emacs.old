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
;; to a folder called 'org'.
;;
;; Once configured, you can send yourself an email and org will automatically capture
;; the contents if it can find a task matching the subject line.

;;; Code:

(require 'cb-lib)
(require 'cb-paths)
(require 'async)
(autoload 'org-insert-link "org")
(autoload 'org-insert-todo-subheading "org")
(autoload 'org-capture-goto-target "org-capture")
(autoload 'org-insert-subheading "org")

;;; Message reading
;;;
;;; The search path is memoised and should be accessed using the accessor function.

(defvar cbom:target-folder nil
  "Folder path to search for messages to use for capturing.")

(defun cbom:target-folder ()
  "IO String

Accessor for the variable of the same name.
Return the path to the 'org' folder in the maildir and memoise."
  (or cbom:target-folder
      (let ((dir (->> (f-join user-home-directory "Maildir")
                   (f-directories)
                   (-mapcat 'f-directories)
                   (--first (s-ends-with? "org" it)))))
        (setq cbom:target-folder dir)
        dir)))

(defun cbom:unprocessed-messages (dir)
  "[FilePath] -> IO [(String, FilePath)]

The unprocessed mail in DIR.
DIR should be an IMAP maildir folder containing a subdir called 'new'."
  (let ((new (f-join dir "new")))
    (when (f-exists? new)
      (-map (-juxt 'f-read-text 'identity)
            (f-files new)))))

;;; Message processing

(defun cbom:message-header-value (header msg)
  "String -> String -> Maybe String"
  (cadr (s-match (eval `(rx bol ,header ":" (* space) (group (* nonl)))) msg)))

(defun cbom:multipart-message? (msg)
  "String -> Bool"
  (s-matches? "multipart/alternative"
              (cbom:message-header-value "content-type" msg)))

(defun cbom:split-message-head-and-body (msg)
  "String -> (String, String)"
  (let ((div (s-index-of "\n\n" msg)))
    (cons (substring msg 0 div) (substring msg div))))

(defun cbom:multipart-body-plaintext-section (msg)
  "String -> String"
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
      (--drop-while (or (s-matches? (rx bol "Content" (* nonl) ":") it)
                        (s-blank? it)))
      (s-join "\n")
      (s-trim)
      (s-chop-suffix "="))))

(cl-defun cbom:parse-message ((msg path))
  "(String, FilePath) -> MessagePlist

Read the string MSG parse interesting data into a plist."
  (list :subject (cbom:message-header-value "subject" msg)
        :filepath path
        :body
        (s-trim
         (if (cbom:multipart-message? msg)
             (cbom:multipart-body-plaintext-section msg)
           (cdr (cbom:split-message-head-and-body msg))))))

;;; Org capture

(defun cbom:capture-keywords ()
  "IO [String]"
  (-map (-compose 's-downcase 'cadr) org-capture-templates))

(defun cbom:capture-candidate? (msg-plist)
  "MessagePlist -> IO Bool"
  (-contains? (cbom:capture-keywords)
              (s-downcase (plist-get msg-plist :subject))))

(defun cbom:capture-template-for-plist (msg-plist)
  "MessagePlist -> IO (String, String, ...)

Find an org capture template corresponding to the subject in MSG-PLIST."
  (--first
   (cl-destructuring-bind (_key title &rest rest_) it
     (equal (s-downcase title)
            (s-downcase (plist-get msg-plist :subject))))
   org-capture-templates))

(defun cbom:growl-notify (msg-plist)
  "MessagePlist -> IO ()"
  (growl (format "%s Captured" (s-capitalize (plist-get msg-plist :subject)))
         (plist-get msg-plist :body)
         (f-join user-emacs-directory "assets" "org_unicorn.png")))

(defun cbom:fetch-html-title (url)
  "String -> IO String"
  (with-current-buffer
      (url-retrieve-synchronously
       (if (s-matches? (rx "http" (? "s") "://") url)
           url
         (s-prepend "http://" url)))
    (cadr (s-match (rx "<title>" (group (* nonl)) "</title>")
                   (buffer-string)))))

(defun cbom:capture-with-template (msg-plist)
  "MessagePlist -> IO ()

Capture the data in MSG-PLIST into the destination in its
correspoding capture template."
  (cl-destructuring-bind (key &rest rest_)
      (cbom:capture-template-for-plist msg-plist)
    (save-excursion
      (org-capture-goto-target key)
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
         ;; Otherwise insert the plain heading.
         (t
          (org-insert-subheading subtree-append)
          (insert heading)))))))

(defun cbom:move-message-to-read (msg-plist)
  "MessagePlist -> IO ()

Move an unread message into the corresponding cur directory."
  (let* ((new (plist-get msg-plist :filepath))
         (cur (f-join (f-parent (f-dirname new)) "cur")))
    (f-move new cur)))

(defun cbom:capture-messages ()
  "Parse and capture unread messages in `cbom:target-folder'.
Captures messages subjects match one of the values in `org-capture-templates'.
Captured messages are marked as read."
  (interactive)
  (save-window-excursion
    (save-excursion
      (--each (->> (cbom:unprocessed-messages (cbom:target-folder))
                (-map 'cbom:parse-message)
                (-filter 'cbom:capture-candidate?))
        (atomic-change-group
          (cbom:capture-with-template it)
          (cbom:growl-notify it)
          (cbom:move-message-to-read it))))))

(defvar cbom:capture-timer
  (run-with-timer 0 10 'cbom:capture-messages))

(provide 'cb-org-email-capture)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org-email-capture.el ends here
