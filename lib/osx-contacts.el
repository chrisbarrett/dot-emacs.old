;;; osx-contacts.el --- Read contact address from Contacts.app

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs,
;; Created: 2013-04-07
;; Last changed: 2013-04-08 18:25:12
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;


;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'bbdb nil t))

(defgroup osx-contacts nil
  "Configuration group for `osx-contacts'."
  :group 'osx-contacts)

(defcustom osx-contacts-sqlite-bin (executable-find "sqlite3")
  "Path to `sqlite3' binary file."
  :group 'osx-contacts
  :type 'string)

(defcustom osx-contacts-base
  "~/Library/Application Support/AddressBook/AddressBook-v22.abcddb"
  "Location of Contacts.app database."
  :group 'osx-contacts
  :type 'string)

(defvar osx-contacts-query
  "SELECT ZABCDRECORD.ZFIRSTNAME,
          ZABCDRECORD.ZLASTNAME,
          ZABCDRECORD.ZNICKNAME,
          (SELECT GROUP_CONCAT(ZABCDEMAILADDRESS.ZADDRESS)
                  FROM ZABCDEMAILADDRESS
                  WHERE ZABCDEMAILADDRESS.ZOWNER = ZABCDRECORD.Z_PK)
           AS EMAIL
    FROM ZABCDRECORD
    WHERE NOT(EMAIL IS NULL);"
  "Query to run")


(defun osx-contacts-parse-buffer ()
  "Parse the result of `osx-contacts-sync' and save result to
`bbdb-file'."
  (let* ((str (buffer-substring-no-properties
               (point-min) (point-max))))
    (with-temp-file (if (boundp 'bbdb-file) bbdb-file "~/.bbdb")
      (insert ";; -*- mode: Emacs-Lisp; coding: utf-8; -*-\n"
              ";;; file-format: 7\n")
      (loop for l in (split-string str "\n" t)
            do (destructuring-bind
                   (first-name name nick email) (split-string l "|")
                 (insert
                  (format
                   "%S\n"
                   (vector first-name
                           name
                           nil ;; middle
                           (when (> (length nick) 0) (list nick)) ;; aka (list)
                           nil ;; Organization (list)
                           nil ;; phone (list of vector ["type" 0 0 0 num])
                           nil ;; address (list of vector [ "type" ("line 1"
                           ;; "line 2") "city" "State" "postcode"
                           ;; "Country"]
                           (split-string email "," t) ;; email (list)
                           nil ;; Last change
                           nil ;; ?
                           ))))))))


(defun osx-contacts-run-sentinel (proc change)
  "`osx-contacts-run' process sentinel."
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
          (cmd-buf (process-get proc :cmd-buf)))
      (if (not (eq 0 status))
          (progn
            (when (process-buffer proc)
              (set-window-buffer (selected-window) cmd-buf))
            (error "OSX Contacts ERROR"))
        (with-current-buffer cmd-buf
          (osx-contacts-parse-buffer))
        (kill-buffer cmd-buf)))))

;;;###autoload
(defun osx-contacts-sync ()
  "Synchronize contacts from Contacts.app to `bbdb'.

Warning, this is a destructive operation. All your existing
contacts would be destroyed."
  (interactive)
  (let* ((cmd-line (list osx-contacts-sqlite-bin
                         (expand-file-name osx-contacts-base)
                         osx-contacts-query))
         (cmd-buf (get-buffer-create " *OSX Contacts*"))
         (proc (apply 'start-process (car cmd-line)
                      cmd-buf (car cmd-line) (cdr cmd-line))))
    (process-put proc :cmd-buf cmd-buf)
    (set-process-sentinel proc 'osx-contacts-run-sentinel)))

(provide 'osx-contacts)

;;; osx-contacts.el ends here
