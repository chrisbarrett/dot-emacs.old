;;; config-org-drill.el --- Configure org-drill

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

;; Configure org-drill

;;; Code:

(require 'utils-common)
(require 'config-orgmode)

(when (cb:install-package 'org-drill-table t)
  (add-hook 'org-ctrl-c-ctrl-c-hook 'org-drill-table-update))

(custom-set-variables
 '(org-drill-save-buffers-after-drill-sessions-p nil))

(defadvice org-drill (after save-buffers activate)
  (org-save-all-org-buffers))

(--each '(org-drill
          org-drill-strip-all-data
          org-drill-cram
          org-drill-tree
          org-drill-resume
          org-drill-merge-buffers
          org-drill-entry
          org-drill-directory
          org-drill-again)
  (autoload it "org-drill" nil t))

(provide 'config-org-drill)

;;; config-org-drill.el ends here
