;;; config-org-table.el --- Configuration for org-table.

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

;; Configuration for org-table.

;;; Code:

(require 'org)

(defun cb-org:recalculate-whole-table ()
  "Recalculate the current table using `org-table-recalculate'."
  (interactive "*")
  (when (org-at-table-p)
    (let ((before (buffer-substring (org-table-begin) (org-table-end))))
      (org-table-recalculate '(16))
      (let ((after (buffer-substring (org-table-begin) (org-table-end))))
        (if (equal before after)
            (message "Table up-to-date")
          (message "Table updated"))))))

(add-hook 'org-ctrl-c-ctrl-c-hook 'cb-org:recalculate-whole-table)

(define-key org-mode-map (kbd "C-c t") 'org-table-recalculate-buffer-tables)

(provide 'config-org-table)

;;; config-org-table.el ends here
