;;; config-csv.el --- Configure CSV mode

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

;; Configure CSV mode

;;; Code:

(require 'utils-common)

(autoload 'csv-mode (f-join cb:lib-dir "csv-mode") "" t)
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

(custom-set-variables
 '(csv-align-style 'auto))

(add-hook 'csv-mode-hook 'csv-align-fields)

(defvar-local cb-csv:aligned? nil)

(defadvice csv-align-fields (after set-aligned activate)
  "Set var for maintaining toggle state."
  (setq cb-csv:aligned? t))

(defadvice csv-unalign-fields (after set-unaligned activate)
  "Set var for maintaining toggle state."
  (setq cb-csv:aligned? nil))

(defun cb-csv:toggle-field-alignment ()
  "Toggle field alignment in the current CSV buffer."
  (interactive)
  (funcall (if cb-csv:aligned? 'csv-unalign-fields 'csv-align-fields)
           t nil nil))

(after 'csv-mode
  (define-key csv-mode-map (kbd "C-c C-t") 'cb-csv:toggle-field-alignment))

(provide 'config-csv)

;;; config-csv.el ends here
