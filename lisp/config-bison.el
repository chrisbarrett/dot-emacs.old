;;; config-bison.el --- Configure bison

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

;; Configure bison

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)

(autoload 'bison-mode "bison-mode")
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))

;;; Commands

(defun cb-bison:m-ret ()
  "Perform a context-sensitive newline action."
  (interactive)
  (cond
   ;; First case after production identifier
   ((s-matches? (rx ":" (* space) eol) (current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "  "))
   ;; Second case
   ((save-excursion
      (forward-line -1)
      (s-matches? (rx ":" (* space) eol) (current-line)))
    (goto-char (line-end-position))
    (newline)
    (insert "| "))
   ;; New case
   ((s-matches? (rx bol (* space) "|") (current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "| "))
   ;; Otherwise open a new line.
   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (when (true? evil-mode)
    (evil-insert-state))

  (bison-format-buffer))

;;; Key bindings

(after 'bison-mode
  (define-key bison-mode-map (kbd "M-RET") 'cb-bison:m-ret)
  (define-key bison-mode-map (kbd "=") (make-smart-op "=")))

(provide 'config-bison)

;;; config-bison.el ends here
