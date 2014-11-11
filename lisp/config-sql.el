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
  (-any? (lambda (opts)
           (thing-at-point-looking-at
            (rx-to-string `(and (regexp ,opts) (* space)))))
         (cb-sql:keywords)))

(defun cb-sql:electric-space ()
  "Upcase the preceding SQL keyword."
  (interactive)
  (let ((in-string-or-comment? (nth 8 (syntax-ppss))))
    (cond (in-string-or-comment?)
          ((cb-sql:after-sql-keyword?)
           (upcase-word -1))))
  (insert " "))

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

  (define-key sql-mode-map (kbd "C-c C-z") 'sql-product-interactive)
  (define-key sql-interactive-mode-map (kbd "C-c C-z") 'cb-sql:switch-back-to-sql)
  )

(provide 'config-sql)

;;; config-sql.el ends here
