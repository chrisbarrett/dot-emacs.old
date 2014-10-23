;;; config-theme.el --- Configure themes and faces

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

;; Configure themes and faces

;;; Code:

(require 'utils-common)
(require 'config-solarized)

;;; Compat

(when (and (<= emacs-major-version 24)
           (< emacs-minor-version 3))
  (defalias 'set-face-bold 'set-face-bold-p))

;;; Advices

(defadvice jit-lock-force-redisplay (around ignore-killed-buffers activate)
  "Do not attempt to run font lock on killed buffers."
  (let ((buf (ad-get-arg 0)))
    (when (buffer-live-p buf)
      ad-do-it)))

(defadvice set-face-font (around ignore-in-term activate)
  "Ignore attempts to change the font in terminals."
  (when (display-graphic-p) ad-do-it))

;;; Font utilities

(defun first-font (&rest fonts)
  "Return the first available font in FONTS."
  (--first (find-font (font-spec :name it)) fonts))

(defun monospace-font ()
  "Retun the monospace type-face name to use for this Emacs session."
  (or (first-font "Source Code Pro" "Menlo" "Consolas" "Inconsolata" "DejaVu Sans Mono"
                  "Ubuntu Mono Regular" "Courier")
      "Menlo"))

;;; Force use of monospace font

(set-frame-font (format "%s 11" (monospace-font)) t)

(hook-fn 'after-make-frame-functions
  (set-frame-font (format "%s 11" (monospace-font)) t
                  (list (car (frame-list)))))

;;; Custom faces

(defface cb:bg-flash
  '((((class color) (background light))
     :background "darkseagreen2")
    (((class color) (background dark))
     :background "royalblue4"))
  "Face for flashing with a green background."
  :group 'cb-faces)

(defface cb:bg-flash-red
  '((t (:background "rosybrown1")))
  "Face for flashing with a red background."
  :group 'cb-faces)

;;; Automatically load last theme.

(defconst cbcl:saved-theme-file (f-join cb:tmp-dir "last-theme")
  "Filepath to a file containing the last selected colour theme.")

(defun cbcl:save-theme-settings (theme)
  "Save THEME to a file at `cbcl:saved-theme-file'."
  (when after-init-time
    (f-write (format "(%s)" theme) 'utf-8 cbcl:saved-theme-file)))

(defun cb-colour:load-last-theme ()
  (condition-case _
      (load cbcl:saved-theme-file nil t t)
    (error (solarized-light))))

(unless (or noninteractive (true? after-init-time))
  (cb-colour:load-last-theme))

;;; Highlight TODO keywords in all modes.

(hook-fn 'prog-mode-hook
  (font-lock-add-keywords
   nil `((,(rx bow (group (or "FIX" "TODO" "FIXME" "HACK" "REFACTOR")) ":")
          1 font-lock-warning-face t))))

(provide 'config-theme)

;;; config-theme.el ends here
