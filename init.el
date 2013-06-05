;;; init.el --- My emacs configuration

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

;; My emacs configuration.

;;; Code:

(message "Emacs %s.%s %s"
         emacs-major-version emacs-minor-version system-configuration)

;;; Disable intrusive GUI elements.

(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(blink-cursor-mode -1)
(menu-bar-mode (if (display-graphic-p) +1 -1))

(defvar cb:use-vim-keybindings? t
  "Set to nil to disable Evil-mode and associated key bindings.")

;;;; Basic paths.

(setq user-emacs-directory (expand-file-name user-emacs-directory))
(defvar user-home-directory (format "%s/" (getenv "HOME")))
(defvar user-dropbox-directory (concat user-home-directory "Dropbox/"))
(add-to-list 'load-path user-dropbox-directory)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(setq-default default-directory user-home-directory)

;;; Configure packages.

(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'bind-key)
  (package-install 'bind-key))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose nil)

(defadvice use-package-ensure-elpa (around ignore-errs activate)
  "Ignore errors caused by package generation."
  (condition-case err
      ad-do-it
    (file-already-exists)))

(use-package diminish
  :ensure t)
(use-package s
  :ensure t)
(use-package dash
  :ensure t)
(use-package cl-lib)
(use-package cb-lib)
(use-package personal-config)
(use-package cb-foundation)
(use-package cb-typefaces)
(use-package cb-modeline)
(use-package cb-mode-groups)
(use-package cb-osx
  :if (equal system-type 'darwin))
(use-package cb-helm)
(use-package cb-ido)
(use-package cb-commands
  :idle (require 'cb-commands)
  :bind
  (("s-f"     . rotate-buffers)
   ("C-x C-o" . other-window))

  :commands
  (select-largest-window
   rotate-buffers
   kill-current-buffer
   clean-buffers
   hide-dos-eol
   last-buffer-for-mode
   insert-timestamp
   indent-buffer
   indent-dwim
   rename-buffer-and-file
   delete-buffer-and-file
   show-autoloads
   insert-shebang
   move-line-up
   move-line-down
   goto-first-occurence
   swap-with-previous-buffer)

  :init
  (progn
    (setq cb:kill-buffer-ignored-list
          '("*scratch*" "*Messages*" "*GROUP*"
            "*shell*" "*eshell*" "*ansi-term*"))
    (add-hook 'find-file-hook 'hide-dos-eol)
    (bind-key "C-c k b" 'clean-buffers)
    (bind-key "C-<up>" 'move-line-up)
    (bind-key "C-<down>" 'move-line-down)
    (bind-key* "C-;" 'swap-with-previous-buffer)
    (define-key prog-mode-map (kbd "M-q") 'indent-dwim))

  :config
  (defadvice rotate-buffers (after select-largest-window activate)
    "Switch to the largest window if using a 2-up window configuration."
    (when (= 2 (length (window-list)))
      (select-largest-window))))
(use-package cb-window-management)
(use-package cb-backups)
(use-package cb-cosmetic)
(use-package cb-colour
  :if (or (daemonp) (display-graphic-p))
  :commands
  (cb:load-theme
   solarized-light
   solarized-dark
   ir-black)
  :init
  (progn
    (setq color-theme-is-global nil)
    (defconst cb:last-theme (concat cb:tmp-dir "last-theme"))

    (condition-case _
        (load cb:last-theme nil t t)
      (solarized-light)
      (error (solarized-light)))

    (hook-fn 'cb:color-theme-changed-hook
      (set-face-font 'default (format "%s 11" (monospace-font)))
      (with-temp-buffer
        (insert (prin1-to-string (list (car args))))
        (write-file cb:last-theme))
      (message nil))))
(use-package cb-web)
(use-package cb-vim
  :if cb:use-vim-keybindings?)
(use-package cb-shell)
(use-package cb-completion)
(use-package cb-dired)
(use-package cb-compilation)
(use-package cb-ctags
  :bind
  (("C-]"     . find-ctag)
   ("C-c C-r" . load-ctags))
  :commands
  (load-ctags
   build-ctags
   visit-ctags
   find-ctag)
  :init
  (progn
    ;; Ensure tags searches are case-sensitive.
    (setq tags-case-fold-search nil)
    (global-set-key (kbd "M-.") 'find-ctag)))
(use-package cb-language-utils)
(use-package cb-markup)
(use-package cb-lisp)
(use-package cb-elisp)
(use-package cb-clojure)
(use-package cb-overtone
  :commands
  (maybe-enable-overtone-mode
   cb:stop-overtone)
  :init     (add-hook 'clojure-mode-hook 'maybe-enable-overtone-mode))
(use-package cb-scheme)
(use-package cb-python)
(use-package cb-ruby)
(use-package cb-haskell)
(use-package cb-clang)
(use-package cb-supercollider)
(use-package cb-git)
(use-package cb-org)
(use-package cb-productivity)
(use-package cb-fortune)

;;; Misc languages
(use-package make-mode
  :defer t
  :config
  (progn
    (add-to-list 'ac-modes 'makefile-mode)
    (hook-fn 'makefile-mode-hook
      (auto-complete-mode t)
      (setq indent-tabs-mode t))))
(use-package json-mode
  :ensure    t
  :commands
  (json-mode
   beautify-json)
  :mode      ("\\.json$" . json-mode)
  :init      (defalias 'format-json 'beautify-json)
  :config    (add-to-list 'ac-modes 'json-mode))
(use-package asm-mode
  :commands asm-mode
  :config
  (progn

    (defun cb:asm-toggling-tab ()
      (interactive)
      (if (equal (line-beginning-position)
                 (progn (back-to-indentation) (point)))
          (indent-for-tab-command)
        (indent-to-left-margin)))

    (defun cb:asm-tab ()
      "Perform a context-sensitive indentation."
      (interactive)
      (if (s-contains? ":" (thing-at-point 'line))
          (indent-to-left-margin)
        (cb:asm-toggling-tab)))

    (defun cb:asm-electric-colon ()
      "Insert a colon, indent, then newline."
      (interactive)
      (atomic-change-group
        (unless (thing-at-point-looking-at (rx ":" (* space) eol))
          (insert ":"))
        (cb:asm-tab)
        (newline-and-indent)))

    (hook-fn 'asm-mode-hook
      (setq tab-width 8)
      (local-set-key (kbd "<tab>") 'cb:asm-tab)
      (local-set-key (kbd ":") 'cb:asm-electric-colon))))
(use-package fsharp-mode
  :ensure   t
  :commands fsharp-mode
  :mode ("\\.fs[ixly]?$" . fsharp-mode)
  :config
  (progn
    (add-to-list 'ac-modes 'fsharp-mode)
    (unless (display-graphic-p)
      (setq fsharp-ac-use-popup nil))
    (add-hook 'fsharp-mode-hook 'electric-indent-mode)
    (add-hook 'fsharp-mode-hook 'electric-layout-mode)))
(use-package conf-mode
  :mode
  ((".gitignore$"  . conf-mode)
   (".gitmodules$" . conf-mode)
   ("ackrc$"       . conf-mode)
   ("Doxyfile$"    . conf-mode))
  :init
  (hook-fn 'cb:conf-modes-hook
    (smartparens-mode +1)))
(use-package sml-mode
  :ensure t
  :mode (("\\.cm"  . sml-cm-mode)
         ("\\.sml" . sml-mode)
         ("\\.sig" . sml-mode)
         ("\\.grm" . sml-yacc-mode))
  :init
  (--each '(".cm/" "CM/")
    (add-to-list 'completion-ignored-extensions it))
  :config
  (setq
   sml-indent-level 2))

;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:

;;; init.el ends here
