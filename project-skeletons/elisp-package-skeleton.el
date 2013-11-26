;;; elisp-package-skeleton.el --- Skeleton for Elisp projects.

;; Copyright (C) 2013 Chris Barrett

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

;; Skeleton for Elisp projects.

;;; Code:

(defun create-elisp-package (name description)
  "Create an Elisp project skeleton with a NAME and DESCRIPTION."
  (interactive "sProject name: \nsDescription: ")
  (with-new-project name "elisp-package"
    ((cons "__project-name__" name)
     (cons "__description__" description))
    (skel-sh "cask")))

(declare-project-skeleton "elisp-package" 'create-elisp-package)

(provide 'elisp-package-skeleton)

;;; elisp-package-skeleton.el ends here
