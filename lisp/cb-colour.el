;;; cb-colour --- Customize colour themes.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Customize colour themes.

;;; Code:

(when (and (<= emacs-major-version 24)
           (< emacs-minor-version 3))
  (defalias 'set-face-bold 'set-face-bold-p))

(defface paren-face
  '((((class color) (background dark))
     (:foreground "grey30"))
    (((class color) (background light))
     (:foreground "grey80")))
  "Face used to dim parentheses."
  :group 'lisp)

(defun solarized-light ()
  (interactive)
  (load-theme 'solarized-light 'no-confirm)

  (when (featurep 'workgroups)
    (set-face-foreground 'wg-divider-face "light slate grey")
    (set-face-foreground 'wg-mode-line-face "light slate grey"))

  (when (featurep 'helm)
    (set-face-background  'helm-selection "white")
    (set-face-underline   'helm-selection nil)
    (set-face-foreground  'helm-selection "black"))

  (when (featurep 'paren)
    (set-face-foreground  'show-paren-match-face "black")
    (set-face-bold        'show-paren-match-face t))

  (set-face-foreground  'paren-face (if (display-graphic-p) "grey80" "blue"))

  (set-face-background  'error "LightPink")
  (set-face-foreground  'error "black")
  (set-face-background  'warning "LightBlue")
  (set-face-foreground  'warning "black"))

(defun solarized-dark ()
  (interactive)
  (load-theme 'solarized-dark 'no-confirm)

  (when (featurep 'workgroups)
    (set-face-foreground 'wg-divider-face "light slate grey")
    (set-face-foreground 'wg-mode-line-face "light slate grey"))

  (when (featurep 'helm)
    (set-face-background  'helm-selection "black")
    (set-face-underline   'helm-selection nil)
    (set-face-foreground  'helm-selection "white"))

  (when (featurep 'paren)
    (set-face-foreground  'show-paren-match-face "white")
    (set-face-bold        'show-paren-match-face t)
    (set-face-background  'show-paren-match-face nil))

  (set-face-foreground  'paren-face (if (display-graphic-p) "grey30" "blue"))

  (set-face-background  'error "Firebrick4")
  (set-face-foreground  'error "gray40")
  (set-face-background  'warning "DarkBlue")
  (set-face-foreground  'warning "gray80"))

(defun ir-black ()
  (interactive)
  (load-theme 'ir-black 'no-confirm)

  (set-face-foreground  'mode-line "gray50")

  (set-face-foreground  'font-lock-doc-face "purple")
  (set-face-italic      'font-lock-doc-string-face t)
  (set-face-foreground  'default "grey50")

  (when (featurep 'workgroups)
    (set-face-foreground 'wg-divider-face "light slate grey")
    (set-face-foreground 'wg-mode-line-face "light slate grey"))

  (when (featurep 'linum)
    (set-face-background  'linum "gray15"))

  (when (featurep 'helm)
    (set-face-foreground  'helm-selection "white")
    (set-face-background  'helm-selection "darkgreen")
    (set-face-underline   'helm-selection nil))

  (when (featurep 'paren)
    (set-face-foreground  'show-paren-match-face "green")
    (set-face-bold        'show-paren-match-face t)
    (set-face-background  'show-paren-match-face nil))

  (when (featurep 'hl-line)
    (set-face-underline   'hl-line nil))

  (set-face-foreground  'paren-face "grey20"))

(provide 'cb-colour)

;;; cb-colour.el ends here
