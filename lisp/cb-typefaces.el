;;; cb-typefaces.el --- Typeface configuration

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0031

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

;; Typeface configuration

;;; Code:

(require 'dash)
(require 'cb-lib)

(defadvice set-face-font (around ignore-in-term activate)
  "Ignore attempts to change the font in terminals."
  (when (display-graphic-p) ad-do-it))

(defun first-font (&rest fonts)
  "Return the first available font in FONTS."
  (--first (find-font (font-spec :name it)) fonts))

(defun serif-font ()
  "Retun the serif type-face name to use for this Emacs session."
  (first-font "Palatino" "Cambria" "Times New Roman"))

(defun sans-serif-font ()
  "Retun the sans-serif type-face name to use for this Emacs session."
  (first-font "Lucida Grande" "Ubuntu Regular" "Segoe UI"
              "Helvetica Neue" "Calibri" "Helvetica" "Verdana" "Arial"))

(defun monospace-font ()
  "Retun the monospace type-face name to use for this Emacs session."
  (or (first-font "Menlo" "Consolas" "Inconsolata" "DejaVu Sans Mono"
                  "Ubuntu Mono Regular" "Courier")
      "Menlo"))

;;; Use typefaces.

(set-frame-font (format "%s 11" (monospace-font)) t)
(hook-fn 'after-make-frame-functions
  (set-frame-font (format "%s 11" (monospace-font)) t
                  (list (car (frame-list)))))

(hook-fn 'text-mode-hook
  "Use a sans-serif font for text-mode."
  (when (equal major-mode 'text-mode)
    (buffer-face-set `(:family ,(sans-serif-font) :height 120))))

(hook-fn 'Info-mode-hook
  (buffer-face-set `(:family ,(serif-font) :height 140)))

(provide 'cb-typefaces)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-typefaces.el ends here
