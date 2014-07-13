;;; config-insertion.el --- Configuration for insertion picker

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

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

;; Configuration for insertion picker

;;; Code:

(require 'utils-common)
(require 'utils-ui)
(require 'utils-shell)

(autoload 'helm "helm")

(defun insert-timestamp ()
  "Read a timestamp from the user and insert it at point."
  (interactive)
  (let ((time (current-time)))
    (helm :prompt "Timestamp: "
          :buffer "*Helm Timestamp*"
          :sources
          `(((name . "Dates")
             (candidates . ,(list
                             (format-time-string "%d-%m-%y" time)
                             (format-time-string "%d-%m-%Y" time)
                             (format-time-string "%d-%m-%Y %H:%M" time)
                             (format-time-string "%d-%m-%Y %I:%M %p" time)))
             (action . insert)
             (volatile))

            ((name . "Times")
             (candidates . ,(list
                             (format-time-string "%X" time)
                             (format-time-string "%I:%M %p" time)
                             (format-time-string "%I:%M:%S %p" time)))
             (action . insert)
             (volatile))

            ((name . "Special")
             (candidates . ,(list
                             (format-time-string "%d %B, %Y" time)
                             (format-time-string "%Y-%m-%dT%H%M%S%z")))
             (action . insert)
             (volatile))))))

(defun cb:filename->interpreter (filename)
  (cdr
   (assoc (file-name-extension filename)
          '(("el" . "emacs")
            ("hs" . "runhaskell")
            ("py" . "python")
            ("rb" . "ruby")
            ("sh" . "bash")))))

(defun insert-shebang (cmd)
  "Insert a shebang line at the top of the current buffer.
Prompt for a command CMD if one cannot be guessed."
  (interactive
   (list (or (cb:filename->interpreter buffer-file-name)
             (read-string "Command name: " nil t))))
  (require 'emr)
  (emr-reporting-buffer-changes "Inserted shebang"
    (save-excursion
      (goto-char (point-min))
      (open-line 2)
      (insert (concat "#!/usr/bin/env " cmd)))))

(defun insert-variable (variable)
  "Insert the value of VARIABLE at point."
  (interactive
   (list
    (intern
     (ido-completing-read
      "Variable: "
      (-map 'symbol-name
            (filter-atoms (-orfn 'custom-variable-p 'special-variable-p)))))))
  (insert (pp-to-string (eval variable))))

(defun make-uuid ()
  "Generate a UUID using the uuid utility."
  (%-string "uuidgen"))

(defun insert-uuid ()
  "Insert a GUID at point."
  (interactive "*")
  (insert (make-uuid)))

(defalias 'insert-guid 'insert-uuid)

(defun insert-lorem-ipsum (n-paragraphs paragraph-length)
  "Insert N-PARAGRAPHS of lorem ipsum text into the current buffer.
PARAGRAPH-LENGTH is one of short, medium, long or verylong."
  (interactive
   (list (read-number "Number of paragraphs: " 3)
         (ido-completing-read "Paragraph length: "
                              '("short" "medium" "long" "verylong"))))
  (let ((url (format "http://loripsum.net/api/%s/%s/plaintext"
                     n-paragraphs paragraph-length)))
    (insert (with-current-buffer (url-retrieve-synchronously url)
              ;; Skip HTTP header.
              (goto-char (point-min))
              (search-forward "\n\n")
              (s-trim (buffer-substring (point) (point-max)))))))

(define-command-picker insertion-picker
  :title "*Insert*"
  :options
  '(("F" "File" insert-file)
    ("L" "Lorem Ipsum" insert-lorem-ipsum)
    ("T" "Timestamp" insert-timestamp)
    ("u" "Unicode Char" insert-char)
    ("U" "UUID" insert-uuid)
    ("V" "File Local Var" add-file-local-variable)
    ("P" "File Local Var (prop line)" add-file-local-variable-prop-line)))

(bind-key* "C-c i" 'insertion-picker)

(provide 'config-insertion)

;;; config-insertion.el ends here
