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

(custom-set-faces
 `(iedit-occurrence ((t (:background
                         ,solarized-hl-orange
                         :foreground
                         "white")))))

;;; Commands

(defun cbiedit:replace-read ()
  (save-excursion
    (iedit-goto-first-occurrence)
    (iedit-replace-occurrences (read-string "Replace in buffer: "))))

(defun cbiedit:number-of-occurrences ()
  "Return the number of active iedit occurrences."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (until (eobp)
        (let ((pos (point))
              (in-occurrence (get-char-property (point) 'iedit-occurrence-overlay-name)))
          (when in-occurrence
            (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name)))
          (setq pos (next-single-char-property-change pos 'iedit-occurrence-overlay-name))
          (goto-char pos)
          (cl-incf n)))
      (1- n))))

(defun cbiedit:restrict-to-region ()
  (iedit-restrict-region (region-beginning) (region-end))
  (message "Restricted to %s matches in region."
           (cbiedit:number-of-occurrences)))

(defun cbiedit:remove-region ()
  (iedit-restrict-region (region-beginning) (region-end) t))

(defun cbiedit:restrict-to-window ()
  (let ((top (save-excursion
               (move-to-window-line 0)
               (point)))
        (bottom (save-excursion
                  (move-to-window-line -1)
                  (point))))
    (iedit-restrict-region top bottom)
    (message "Restricted to %s matches in visible area."
             (cbiedit:number-of-occurrences))))

(defun cbiedit:replace-in-region ()
  (cbiedit:restrict-to-region)
  (cbiedit:replace-read))

;;; Picker

(define-command-picker iedit-picker
  :title "*iedit*"
  :options
  '(("g" "First occurrence" iedit-goto-first-occurrence)
    ("G" "Last occurrence" iedit-goto-last-occurrence)
    ("p" "Previous occurrence" iedit-prev-occurrence)
    ("n" "Next occurrence" iedit-next-occurrence)
    ("r" "Replace" cbiedit:replace-read)
    ("k" "Delete Matches" iedit-delete-occurrences)

    ("l" "Restrict (line)" iedit-restrict-current-line)
    ("R" "Restrict (region)" cbiedit:restrict-to-region :when region-active-p)
    ("w" "Restrict (window)" cbiedit:restrict-to-window)
    ("x" "Remove (region)" cbiedit:remove-region :when region-active-p)
    ("f" "Restrict (function)" iedit-restrict-function :when (lambda () (thing-at-point 'defun)))

    ("c" "Toggle Case-Sensitivity" iedit-toggle-case-sensitive)
    ("t" "Toggle at Point" iedit-toggle-selection)
    ("d" "Done" iedit-done)))

;;; Key bindings

(bind-key "M-r" 'iedit-mode)

(after 'iedit
  (bind-key "C-c r" 'iedit-picker iedit-mode-keymap))

(provide 'config-iedit)

;;; config-iedit.el ends here
