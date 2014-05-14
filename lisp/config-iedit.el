;;; config-iedit.el --- Configuration for iedit

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

;; Configuration for iedit

;;; Code:

(require 'utils-common)
(require 'utils-ui)

(cb:install-package 'iedit)

(bind-key "M-r" 'iedit-mode)

(defun cbiedit:replace-read ()
  (iedit-replace-occurrences (read-string "Replace in buffer: ")))

(defun cbiedit:restrict-to-region ()
  (iedit-restrict-region (region-beginning) (region-end)))

(defun cbiedit:remove-region ()
  (iedit-restrict-region (region-beginning) (region-end) t))

(defun cbiedit:replace-in-region ()
  (cbiedit:restrict-to-region)
  (cbiedit:replace-read))

(define-command-picker iedit-picker
  :title "*iedit*"
  :options
  '(("e" "Expand"
     iedit-expand-by-a-line
     :unless region-active-p)

    ("p" "Expand (up)"
     iedit-expand-up-a-line
     :unless region-active-p)

    ("n" "Expand (down)"
     iedit-expand-down-a-line
     :unless region-active-p)

    ("r" "Replace (in region)"
     cbiedit:replace-in-region
     :when region-active-p)

    ("r" "Replace"
     cbiedit:replace-read
     :unless region-active-p)

    ("k" "Delete Matches"
     iedit-delete-occurrences
     :unless region-active-p)

    ("l" "Restrict (line)"
     iedit-restrict-current-line
     :unless region-active-p)

    ("R" "Restrict (region)"
     cbiedit:restrict-to-region
     :when region-active-p)

    ("x" "Remove (region)"
     cbiedit:remove-region
     :when region-active-p)
    ("f" "Restrict (function)"
     iedit-restrict-function
     :when (lambda () (thing-at-point 'defun)))

    ("c" "Toggle Case-Sensitivity" iedit-toggle-case-sensitive)
    ("t" "Toggle at Point" iedit-toggle-selection)
    ("d" "Done" iedit-done)))

(after 'iedit
  (bind-key "M-r" 'iedit-picker iedit-mode-keymap))


(provide 'config-iedit)

;;; config-iedit.el ends here
