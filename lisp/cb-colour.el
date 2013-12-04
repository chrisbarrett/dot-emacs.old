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

(require 'use-package)
(require 'cb-foundation)
(require 'cb-typefaces)

(defmacro cb:define-theme (sym &rest body)
  "Define a theme."
  (declare (indent 1))
  `(defun ,sym ()
     (interactive)
     ,@body
     (run-hook-with-args 'cb:color-theme-changed-hook ',sym)))

(defvar solarized-hl-yellow    "#b58900")
(defvar solarized-hl-orange    "#cb4b16")
(defvar solarized-hl-red       "#dc322f")
(defvar solarized-hl-magenta   "#d33682")
(defvar solarized-hl-violet    "#6c71c4")
(defvar solarized-hl-blue      "#268bd2")
(defvar solarized-hl-cyan      "#2aa198")
(defvar solarized-hl-green     "#859900")

(configuration-group
  :when (or (daemonp) (display-graphic-p))

  (use-package solarized-theme
    :ensure t
    :defer t)

  (use-package color-theme-sanityinc-tomorrow
    :ensure t
    :defer t
    :config
    (--each '("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016"
              "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58"
              "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
              "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a")
      (add-to-list 'custom-safe-themes it)))

  (when (and (<= emacs-major-version 24)
             (< emacs-minor-version 3))
    (defalias 'set-face-bold 'set-face-bold-p))

  (defvar cb:color-theme-changed-hook nil
    "Run when the theme is changed. Passes the command that was used an argument.")

  (defmacro cb:define-theme (sym &rest body)
    "Define a theme."
    (declare (indent 1))
    `(defun ,sym ()
       (interactive)
       ,@body
       (run-hook-with-args 'cb:color-theme-changed-hook ',sym)))

  (cb:define-theme solarized-light
    (load-theme 'solarized-light 'no-confirm)
    (cb-colour:common-setup)

    (after 'org
      (set-face-background 'org-block-begin-line "#f8f1dc")
      (set-face-background 'org-block-end-line "#f8f1dc")
      (set-face-background 'org-block-background "#f8f1dc"))

    (after 'helm
      (set-face-background  'helm-selection "white")
      (set-face-foreground  'helm-selection "black"))

    (after 'eval-sexp-fu
      (set-face-background 'eval-sexp-fu-flash "#eee8d5")
      (set-face-foreground 'eval-sexp-fu-flash solarized-hl-cyan))

    (after 'parenface-plus
      (set-face-foreground  'paren-face "grey80")))

  (defalias 'light 'solarized-light)

  (cb:define-theme solarized-dark
    (load-theme 'solarized-dark 'no-confirm)
    (cb-colour:common-setup)
    (cb-colour:dark-setup)
    (after 'org
      (set-face-background 'org-block-end-line "#11303b")
      (set-face-background 'org-block-begin-line "#11303b")
      (set-face-background 'org-block-background "#11303b")))

  (defalias 'dark 'solarized-dark)

  (cb:define-theme tomorrow-night
    (color-theme-sanityinc-tomorrow-night)
    (cb-colour:common-setup)
    (cb-colour:dark-setup)
    (after 'org
      (set-face-background 'org-block-end-line "#222727")
      (set-face-background 'org-block-begin-line "#222727")
      (set-face-background 'org-block-background "#222727")))

  (defun cb-colour:dark-setup ()
    "Perform customisation common to dark colour themes."
    (after 'helm
      (set-face-background  'helm-selection "black")
      (set-face-foreground  'helm-selection "white"))

    (after 'eval-sexp-fu
      (set-face-background 'eval-sexp-fu-flash "#073642")
      (set-face-foreground 'eval-sexp-fu-flash-error solarized-hl-cyan))

    (after 'parenface-plus
      (set-face-foreground  'paren-face "#505070")))

  (defun cb-colour:common-setup ()
    "Perform customisation common to all themes."
    (set-face-underline 'hl-line nil)
    (set-face-font 'default (format "%s 11" (monospace-font)))
    (after 'eval-sexp-fu
      (set-face-background 'eval-sexp-fu-flash-error solarized-hl-orange))
    (after 'helm
      (set-face-underline   'helm-selection nil))
    (after 'smartparens
      (set-face-foreground  'sp-show-pair-match-face "#002b36")
      (set-face-background  'sp-show-pair-match-face "white")
      (set-face-bold        'sp-show-pair-match-face t))
    (after 'iedit
      (set-face-attribute 'iedit-occurrence nil :underline solarized-hl-orange))
    (after 'org
      (set-face-underline  'org-block-begin-line t)
      (set-face-attribute  'org-block-end-line nil :overline t)
      (set-face-background 'org-hide 'unspecified)
      (set-face-foreground 'org-document-info-keyword 'unspecified)
      (set-face-italic 'org-meta-line nil)
      (set-face-attribute 'org-document-info-keyword nil :inherit 'org-meta-line)
      (--each (--filter-atoms (and (s-starts-with? "org-level-" (symbol-name it))
                                   (facep it)))
        (unless (equal 'org-level-1 it)
          (set-face-bold it nil))
        (set-face-font it (monospace-font)))))

  (defalias 'black 'tomorrow-night)

  ;; Write current theme to disk whenever the colour theme is changed so it can be
  ;; reloaded on startup.

  (defconst cbcl:saved-theme-file (concat cb:tmp-dir "last-theme")
    "Filepath to a file containing the last selected colour theme.")

  (defun cbcl:save-theme-settings (theme)
    "Save THEME to a file at `cbcl:saved-theme-file'."
    (when after-init-time
      (f-write (format "(%s)" theme) 'utf-8 cbcl:saved-theme-file)))

  (add-hook 'cb:color-theme-changed-hook 'cbcl:save-theme-settings)

  ;; Load the last colour theme when starting emacs.

  (defun cb-colour:load-last-theme ()
    (condition-case _
        (load cbcl:saved-theme-file nil t t)
      (error (solarized-light))))

  (unless (true? after-init-time)
    (cb-colour:load-last-theme))

  )

(provide 'cb-colour)

;;; cb-colour.el ends here
