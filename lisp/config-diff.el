;;; config-diff.el --- Configuration for diff and ediff.

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

;; Configuration for diff and ediff.

;;; Code:

(require 'utils-common)
(require 'config-evil)

(custom-set-variables
 '(diff-switches "-u")
 '(ediff-window-setup-function 'ediff-setup-windows-plain))

(defun cb-diff:close ()
  (interactive)
  (when (> (length (window-list)) 1)
    (kill-buffer-and-window)))

(after 'diff-mode
  (define-key diff-mode-map (kbd "q") 'cb-diff:close))

(defun cb-ediff:prepare-buffer ()
  "Prepare the current buffer for ediff."
  (cond ((derived-mode-p 'org-mode)
         (visible-mode 1))))

(add-hook 'ediff-prepare-buffer-hook 'cb-ediff:prepare-buffer)

(provide 'config-diff)

;;; config-diff.el ends here
