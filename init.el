;;; init.el --- Load org-babel to bootstrap configuration

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

;; My configuration is managed in a number of org-mode files in this directory.
;; This file is responsible for initialising org-babel before loading those
;; files.

;;; Code:

;; Initialise packages and install org-mode.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))
(unless (package-installed-p 'org-plus-contrib) (package-install 'org-plus-contrib))

(require 'ob-tangle)

(defun tangle-and-load-config-files ()
  "Tangle and reload configuration files."
  (interactive)
  (message "Loading config files...")
  (dolist (f (list "config-base.org" "config-orgmode.org" "config-languages.org"))
    (message "Loading %s" f)
    (org-babel-load-file (concat user-emacs-directory f)))
  (message "Loading config files...Done"))

(tangle-and-load-config-files)

(provide 'init)

;;; init.el ends here
