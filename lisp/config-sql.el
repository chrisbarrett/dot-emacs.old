;;; config-sql.el --- Configuration for SQL modes.

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

;; Configuration for SQL modes.

;;; Code:

(require 'utils-common)
(require 'utils-buffers)
(require 'sql-indentation)

;;; Configure smart operators

(super-smart-ops-configure-for-mode 'sql-mode
  :custom '(("," . cb:comma-then-space)))

(super-smart-ops-configure-for-mode 'sql-interactive-mode
  :custom '(("," . cb:comma-then-space)))

;;; Automatically upcase SQL keywords on SPC

(defun cb-sql:keywords ()
  "Produce a regexp matching SQL keywords for the current SQL product."
  (-map 'car
        (or (true? sql-mode-font-lock-keywords)
            (sql-add-product-keywords (or (true? sql-product) 'ansi) nil))))

(defun cb-sql:after-sql-keyword? ()
  "Non-nil if point is after an SQL keyword."
  (save-restriction
    (narrow-to-region (line-beginning-position) (point))
    (-any? (lambda (opts)
             (thing-at-point-looking-at
              (rx-to-string `(and symbol-start (regexp ,opts) symbol-end (* space)))))
           (cb-sql:keywords))))

(defun cb-sql:upcase-preceding-keyword ()
  "Upcase the preceding SQL keyword unless point is in a string or comment."
  (let ((in-string-or-comment? (nth 8 (syntax-ppss))))
    (cond (in-string-or-comment?)
          ((cb-sql:after-sql-keyword?)
           (upcase-word -1)))))

(defun cb-sql:electric-space ()
  "Upcase the preceding SQL keyword and insert a space."
  (interactive)
  (cb-sql:upcase-preceding-keyword)
  (insert " "))

(defun cb-sql:comment-indent-newline ()
  "Upcase the preceding SQL keyword and insert a new line."
  (interactive)
  (cb-sql:upcase-preceding-keyword)
  (comment-indent-new-line))

;;; Interactive buffer

(defun cb-sql:switch-back-to-sql ()
  "Switch to the last SQL buffer."
  (interactive)
  (-when-let (buf (--first-buffer (derived-mode-p 'sql-mode)))
    (pop-to-buffer buf)))

;;; Set key bindings

(after 'sql
  (define-key sql-mode-map (kbd "SPC") 'cb-sql:electric-space)
  (define-key sql-interactive-mode-map (kbd "SPC") 'cb-sql:electric-space)
  (define-key sql-mode-map (kbd "RET") 'cb-sql:comment-indent-newline)

  (define-key sql-mode-map (kbd "C-c C-z") 'sql-product-interactive)
  (define-key sql-interactive-mode-map (kbd "C-c C-z") 'cb-sql:switch-back-to-sql)

  (define-key sql-interactive-mode-map (kbd "<backspace>") 'sp-backward-delete-char)
  )

(provide 'config-sql)

;;; config-sql.el ends here
