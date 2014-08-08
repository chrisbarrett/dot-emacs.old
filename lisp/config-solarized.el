;;; config-solarized.el --- Configuration for solarized color theme.

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

;; Configuration for solarized color theme.

;;; Code:

(cb:install-package 'solarized-theme)

(defvar solarized-hl-yellow    "#b58900")
(defvar solarized-hl-orange    "#cb4b16")
(defvar solarized-hl-red       "#dc322f")
(defvar solarized-hl-magenta   "#d33682")
(defvar solarized-hl-violet    "#6c71c4")
(defvar solarized-hl-blue      "#268bd2")
(defvar solarized-hl-cyan      "#2aa198")
(defvar solarized-hl-green     "#859900")

(defun solarized-light ()
  "Switch theme to solarized light."
  (interactive)
  (cbcl:save-theme-settings 'solarized-light)
  (load-theme 'solarized-light 'no-confirm)
  (cb-colour:common-setup)

  (after 'org
    (set-face-background 'org-block-begin-line "#f8f1dc")
    (set-face-background 'org-block-end-line "#f8f1dc")
    (set-face-background 'org-block-background "#f8f1dc")))

(defun solarized-dark ()
  "Switch theme to solarized dark."
  (interactive)
  (cbcl:save-theme-settings 'solarized-dark)
  (load-theme 'solarized-dark 'no-confirm)
  (cb-colour:common-setup)

  (after 'org
    (set-face-background 'org-block-end-line "#11303b")
    (set-face-background 'org-block-begin-line "#11303b")
    (set-face-background 'org-block-background "#11303b")))

(defalias 'light 'solarized-light)
(defalias 'dark 'solarized-dark)


(provide 'config-solarized)

;;; config-solarized.el ends here
