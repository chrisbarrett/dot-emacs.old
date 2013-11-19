;;; cb-asm.el --- Configuration of assembler mode.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130627.0021

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

;; Configuration of assembler mode.

;;; Code:

(require 'use-package)
(require 'cb-lib)
(autoload 's-contains\? "s")
(autoload 'thing-at-point-looking-at "thingatpt")


(after 'smart-operator
  (hook-fn 'asm-mode-hook
    (smart-insert-operator-hook)
    (local-unset-key (kbd "%"))
    (local-unset-key (kbd "-"))
    (local-unset-key (kbd "."))))

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

(use-package asm-mode
  :commands asm-mode
  :config
  (hook-fn 'asm-mode-hook
    (setq tab-width 8)
    (local-set-key (kbd "<tab>") 'cb:asm-tab)
    (local-set-key (kbd ":") 'cb:asm-electric-colon)))

(provide 'cb-asm)

;; Local Variables:
;; End:

;;; cb-asm.el ends here
