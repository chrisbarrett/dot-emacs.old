;;; config-tramp.el --- Configuration for tramp.

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

;; Configuration for tramp.

;;; Code:

(require 'utils-common)

(defvar tramp-bkup-backup-directory-info nil
  "Variable that must be defined to work around tramp bug.")

;;; Correctly shut down tramp when exiting Emacs.

(defun cb-tramp:clean-buffers-on-exit ()
  "Clean all tramp buffers."
  (ignore-errors
    (when (fboundp 'tramp-cleanup-all-buffers)
      (tramp-cleanup-all-buffers))))

(add-hook 'kill-emacs-hook 'cb-tramp:clean-buffers-on-exit)

;;; Define command to edit file with sudo.

(cl-defun sudo-edit (&optional (file (buffer-file-name)))
  "Edit FILE with sudo if permissions require it."
  (interactive)
  (when file
    (cond
     ((f-dir? file)
      (error "%s is a directory" file))

     ((file-writable-p file)
      (error "%s: sudo editing not needed" file))

     ;; Prompt user whether to escalate. Ensure the tramp connection is cleaned
     ;; up afterwards.
     ((and (yes-or-no-p "Edit file with sudo?  ")
           (find-alternate-file (concat "/sudo:root@localhost:" file)))
      (add-hook 'kill-buffer-hook 'tramp-cleanup-this-connection nil t)))))

(bind-key* "C-x e" 'sudo-edit)

;;; Edit files with sudo if not writable.

(defun maybe-sudo-edit ()
  "Attempt to sudo-edit if the current file is not writeable."
  (let ((dir (f-dirname (buffer-file-name))))

    (when (or (and (not (f-writable? (buffer-file-name)))
                   (f-exists? (buffer-file-name)))

              (and dir
                   (f-exists? dir)
                   (not (f-writable? dir))))
      (sudo-edit))))

(add-hook 'find-file-hook 'maybe-sudo-edit)

(provide 'config-tramp)

;;; config-tramp.el ends here
