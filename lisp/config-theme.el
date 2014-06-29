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

(setq font-lock-maximum-decoration t)

(defadvice jit-lock-force-redisplay (around ignore-killed-buffers activate)
  "Do not attempt to run font lock on killed buffers."
  (let ((buf (ad-get-arg 0)))
    (when (buffer-live-p buf)
      ad-do-it)))

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

(set-frame-font (format "%s 11" (monospace-font)) t)

(hook-fn 'after-make-frame-functions
  (set-frame-font (format "%s 11" (monospace-font)) t
                  (list (car (frame-list)))))

(when (and (<= emacs-major-version 24)
           (< emacs-minor-version 3))
  (defalias 'set-face-bold 'set-face-bold-p))

(defvar solarized-hl-yellow    "#b58900")
(defvar solarized-hl-orange    "#cb4b16")
(defvar solarized-hl-red       "#dc322f")
(defvar solarized-hl-magenta   "#d33682")
(defvar solarized-hl-violet    "#6c71c4")
(defvar solarized-hl-blue      "#268bd2")
(defvar solarized-hl-cyan      "#2aa198")
(defvar solarized-hl-green     "#859900")

(defface intense-flash
  `((((class color) (background dark))
     (:bold t :background "#073642" :foreground ,solarized-hl-cyan))
    (((class color) (background light))
     (:bold t :background "#eee8d5" :foreground ,solarized-hl-cyan)))
  "Face for intense highlighted text."
  :group 'cb-faces)

(defconst cbcl:saved-theme-file (f-join cb:tmp-dir "last-theme")
  "Filepath to a file containing the last selected colour theme.")

(defun cbcl:save-theme-settings (theme)
  "Save THEME to a file at `cbcl:saved-theme-file'."
  (when after-init-time
    (f-write (format "(%s)" theme) 'utf-8 cbcl:saved-theme-file)))

(unless noninteractive
  (cb:install-package 'solarized-theme))

(defun cb-colour:common-setup ()
  "Perform customisation common to all themes."

  (when (featurep 'hl-line)
    (set-face-underline 'hl-line nil))

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
    (set-face-attribute 'iedit-occurrence nil :bold t)
    (set-face-attribute 'iedit-occurrence nil :foreground solarized-hl-red))

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

(defun solarized-light ()
  "Switch theme to solarized light."
  (interactive)
  (cbcl:save-theme-settings 'solarized-light)
  (load-theme 'solarized-light 'no-confirm)
  (cb-colour:common-setup)

  (after 'org
    (set-face-background 'org-block-begin-line "#f8f1dc")
    (set-face-background 'org-block-end-line "#f8f1dc")
    (set-face-background 'org-block-background "#f8f1dc"))

  (after 'ledger-fonts
    (set-face-background 'ledger-font-xact-highlight-face "#eee8d5")
    (set-face-background 'ledger-occur-xact-face "#eee8d5"))

  (after 'shm
    (set-face-background 'shm-current-face  "#e9f2c5")
    (set-face-background 'shm-quarantine-face  "#fee8e5"))

  (after 'proof-faces
    (set-face-background 'proof-locked-face nil)
    (set-face-background 'proof-error-face  "#fee8e5"))

  (after 'coq-db
    (set-face-background 'coq-cheat-face "#fee8e5"))

  (after 'helm
    (set-face-background  'helm-selection "white")
    (set-face-foreground  'helm-selection "black"))

  (after 'parenface
    (set-face-foreground  'parenface-paren-face "grey80")))

(defun solarized-dark ()
  "Switch theme to solarized dark."
  (interactive)
  (cbcl:save-theme-settings 'solarized-dark)
  (load-theme 'solarized-dark 'no-confirm)
  (cb-colour:common-setup)

  (after 'helm
    (set-face-background  'helm-selection "black")
    (set-face-foreground  'helm-selection "white"))

  (after 'ledger-fonts
    (set-face-background 'ledger-font-xact-highlight-face "#073642")
    (set-face-background 'ledger-occur-xact-face "#073642"))

  (after 'parenface
    (set-face-foreground  'parenface-paren-face "#505070"))

  (after 'shm
    (set-face-background 'shm-current-face "#01304b")
    (set-face-background 'shm-quarantine-face "#51202b"))

  (after 'proof-faces
    (set-face-background 'proof-locked-face nil)
    (set-face-background 'proof-error-face "#51202b"))

  (after 'coq-db
    (set-face-background 'coq-cheat-face "#51202b"))

  (after 'org
    (set-face-background 'org-block-end-line "#11303b")
    (set-face-background 'org-block-begin-line "#11303b")
    (set-face-background 'org-block-background "#11303b")))

(defalias 'light 'solarized-light)
(defalias 'dark 'solarized-dark)

(defun cb-colour:load-last-theme ()
  (condition-case _
      (load cbcl:saved-theme-file nil t t)
    (error (solarized-light))))

(unless (or noninteractive (true? after-init-time))
  (cb-colour:load-last-theme))

(hook-fn 'prog-mode-hook
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(provide 'config-theme)

;;; config-theme.el ends here
