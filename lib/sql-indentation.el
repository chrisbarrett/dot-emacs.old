;;; sql-indentation.el --- Indentation for SQL code.

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

;; Indentation for SQL code. Adapted from https://gist.github.com/stuntgoat/8912558

;;; Code:

(defun sql-indentation--previous-indent-level ()
  "Return the column for the previous indentation level."
  (save-excursion
    (move-beginning-of-line nil)
    (skip-chars-backward "\n \t")
    (back-to-indentation)
    (current-column)))

(defun sql-indentation--indent-level ()
  "Return column for the current indentation level."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun sql-indentation--indent-level-to-pos ()
  "Return point at current indentation."
  (save-excursion
    (move-to-column (sql-indentation--indent-level))
    (point)))

(defun sql-indentation--col-to-line-pos (col)
  "Return the point at COL on the current line."
  (save-excursion
    (move-to-column col)
    (point)))

(defun sql-indentation--indent-to-col (col)
  "Move the line to COL; fill with all spaces if moving forward."
  (interactive "p")
  (let ((level-pt (sql-indentation--indent-level-to-pos))
		(level-col (sql-indentation--indent-level)))
    (cond
     ((zerop col)
      ;; delete to beginning of line or do nothing
      (unless (zerop level-col)
        (delete-region level-pt (sql-indentation--col-to-line-pos 0))))

     ((< col level-col)
      ;; delete from our current point BACK to col
      (delete-region (sql-indentation--col-to-line-pos col) level-pt))

     ((> col level-col)
      ;; delete all text from indent to beginning of line
      (delete-region level-pt (sql-indentation--col-to-line-pos 0))
      (move-beginning-of-line nil)
      ;; add spaces forward
      (insert (make-string col ?\s))))))

(defun sql-indentation-cycle ()
  "Cycle through indentation levels."
  (let ((previous (sql-indentation--previous-indent-level))
        (current (sql-indentation--indent-level)))
    (cond ((= previous current)
		   (sql-indentation--indent-to-col (+ current tab-width)))

          ((> current previous)
           ;; exactly at one indentation forward from previous lines indent
           (if (= tab-width (- current previous))
               (sql-indentation--indent-to-col 0)
             (sql-indentation--indent-to-col previous)))

          (t
           (sql-indentation--indent-to-col (+ current tab-width))))))

(defun sql-indentation-setup ()
  "Configure the current `sql-mode' buffer to use custom indentation commands."
  (setq-local indent-line-function 'sql-indentation-cycle))

(add-hook 'sql-mode-hook 'sql-indentation-setup)

(provide 'sql-indentation)

;;; sql-indentation.el ends here
