;;; config-org-archive.el --- Configure org-archive

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

;; Configure org-archive

;;; Code:

(require 'org)

(custom-set-variables
 '(org-archive-default-command 'cb-org:archive-done-tasks))

(defadvice org-archive-subtree
    (before add-inherited-tags-before-org-archive-subtree activate)
  "Add inherited tags before org-archive-subtree."
  (org-set-tags-to (org-get-tags-at)))

(defun cb-org:archive-done-tasks ()
  (interactive)
  (atomic-change-group
    (org-map-entries (lambda ()
                       ;; Ensure point does not move past the next item to
                       ;; archive.
                       (setq org-map-continue-from (point))
                       (org-archive-subtree))
                     "/DONE|PAID|VOID|CANCELLED" 'tree)))

(provide 'config-org-archive)

;;; config-org-archive.el ends here
