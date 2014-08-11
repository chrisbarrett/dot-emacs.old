;;; config-dired.el --- Configuration for dired

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

;; Configuration for dired

;;; Code:

(require 'utils-common)

(cb:install-package 'dired-details t)
(dired-details-install)
(require 'dired)
(require 'dired-x)

(custom-set-variables
 '(dired-auto-revert-buffer t)
 '(dired-listing-switches "-al --group-directories-first")
 '(dired-compress-file-suffixes
   '(("\\.zip\\'" ".zip" "unzip")
     ("\\.gz\\'" "" "gunzip")
     ("\\.tgz\\'" ".tar" "gunzip")
     ("\\.Z\\'" "" "uncompress")
     ("\\.z\\'" "" "gunzip")
     ("\\.dz\\'" "" "dictunzip")
     ("\\.tbz\\'" ".tar" "bunzip2")
     ("\\.bz2\\'" "" "bunzip2")
     ("\\.xz\\'" "" "unxz")
     ("\\.tar\\'" ".tgz" nil)))
 '(dired-bind-jump nil)
 '(dired-omit-files
   (regexp-opt
    (list "^\\.?#" "^\\.$" "^\\.\\.$" "\\.DS_Store$" "\\$RECYCLE.BIN")))
 '(dired-details-hidden-string "... "))

(add-hook 'dired-mode-hook 'dired-omit-mode)

(defun cb:line-is-dired-header? ()
  (equal 'dired-header
         (ignore-errors
           (save-excursion
             (move-to-column 3)
             (face-at-point)))))

;;; Hl-line compat

(defadvice global-hl-line-highlight (around suppress-on-subdir-header activate)
  "Do not highlight the line if looking at a dired header."
  (if (and (derived-mode-p 'dired-mode) (cb:line-is-dired-header?))
      (global-hl-line-unhighlight)
    ad-do-it))

(defadvice hl-line-highlight (around suppress-on-subdir-header activate)
  "Do not highlight the line if looking at a dired header."
  (if (and (derived-mode-p 'dired-mode) (cb:line-is-dired-header?))
      (hl-line-unhighlight)
    ad-do-it))

;;; Key bindings

(define-key dired-mode-map (kbd "M-N") 'dired-next-subdir)
(define-key dired-mode-map (kbd "M-P") 'dired-prev-subdir)

(provide 'config-dired)

;;; config-dired.el ends here
