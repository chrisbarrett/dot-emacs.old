;;; config-asm.el --- Configure asm

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

;; Configure asm

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)

(put 'asm-mode 'tab-width 8)

(declare-smart-ops 'asm-mode
  :rem '("%" "-" "."))

(defun cb:asm-toggling-tab ()
  (interactive)
  (if (equal (line-beginning-position)
             (progn (back-to-indentation) (point)))
      (indent-for-tab-command)
    (indent-to-left-margin)))

(defun cb:asm-tab ()
  "Perform a context-sensitive indentation."
  (interactive)
  (if (s-contains? ":" (thing-at-point 'line))
      (indent-to-left-margin)
    (cb:asm-toggling-tab)))

(defun cb:asm-electric-colon ()
  "Insert a colon, indent, then newline."
  (interactive)
  (atomic-change-group
    (unless (thing-at-point-looking-at (rx ":" (* space) eol))
      (insert ":"))
    (cb:asm-tab)
    (newline-and-indent)))

(after 'asm-mode
  (define-key asm-mode-map (kbd "<tab>") 'cb:asm-tab)
  (define-key asm-mode-map (kbd ":") 'cb:asm-electric-colon))

(provide 'config-asm)

;;; config-asm.el ends here
