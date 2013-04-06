;;; init --- My emacs configuration

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

;;; Code:

;;; Disable intrusive GUI elements.
(scroll-bar-mode -1)
(menu-bar-mode   -1)
(tool-bar-mode   -1)

;;; Describe me.
(setq user-full-name    "Chris Barrett"
      user-mail-address "chris.d.barrett@me.com")

;;; ----------------------------------------------------------------------------
;;; Initialize packages.

(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; ----------------------------------------------------------------------------
;;; Load packages.

(require 'bind-key (concat user-emacs-directory "lib/use-package/bind-key.el"))
(require 'use-package (concat user-emacs-directory "lib/use-package/use-package.el"))

(use-package cl-lib)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package cb-macros
  :init
  (progn
    (cb:define-path cb:lib-dir  "lib/" t)
    (cb:define-path cb:lisp-dir "lisp/" t)
    (cb:define-path cb:tmp-dir  "tmp/")
    (cb:define-path cb:bin-dir  "bin/")
    (cb:define-path cb:etc-dir  "etc/")
    (cb:define-path cb:yasnippet-dir "snippets/")
    (cb:define-path cb:backups-dir   "backups/")
    (cb:define-path cb:autosaves-dir "tmp/autosaves/")
    (cb:define-path cb:rsense-home   "bin/rsense-0.3/")))

(use-package helm
  :ensure t)

(use-package ido
  :ensure t
  :config
  (progn
    (setq ido-enable-prefix nil
          ido-save-directory-list-file (concat cb:tmp-dir "ido.last")
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-max-prospects 10
          ido-default-file-method 'selected-window)

    (ido-mode +1)
    (icomplete-mode +1)

    (use-package ido-hacks
      :ensure t)

    (use-package ido-ubiquitous
      :ensure t
      :config (ido-ubiquitous-mode +1))

    (use-package idomenu
      :ensure t
      :commands (idomenu))

    (add-to-list 'ido-ignore-buffers "*helm mini*")
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind   (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)))

(use-package popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer
        popwin:special-display-config
        '(("*Help*"  :height 30 :stick t)
          ("*Completions*" :noselect t)
          ("*compilation*" :noselect t)
          ("*Messages*" :height 30)
          ("*Occur*" :noselect t)
          ("\\*Slime Description.*" :noselect t :regexp t :height 30)
          ("*magit-commit*" :noselect t :height 40 :width 80)
          ("*magit-diff*" :noselect t :height 40 :width 80)
          ("*magit-edit-log*" :noselect t :height 15 :width 80)
          ("\\*Slime Inspector.*" :regexp t :height 30)
          ("*Ido Completions*" :noselect t :height 30)
          ("*eshell*" :height 30)
          ("\\*ansi-term\\*.*" :regexp t :height 30)
          ("*shell*" :height 30)
          (".*overtone.log" :regexp t :height 30)
          ("*gists*" :height 30)
          ("*sldb.*":regexp t :height 30))))

(use-package saveplace
  :config
  (progn
    (setq save-place-file (concat cb:tmp-dir "saved-places"))
    (setq-default save-place t)))

(use-package paren
  :config (show-paren-mode +1))

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat cb:tmp-dir "recentf")
          recentf-max-saved-items 200
          recentf-exclude '(".newsrc"
                            "-autoloads.el"
                            "recentf"
                            ".ido.last"
                            "TAGS"
                            ".gz"))
    (recentf-mode +1)))

(use-package savehist
  :config
  (progn
    (setq savehist-additional-variables '(search ring regexp-search-ring)
          savehist-autosave-interval    60
          savehist-file                 (concat cb:tmp-dir "savehist"))
    (savehist-mode +1)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config   (global-undo-tree-mode +1))

(use-package window-number
  :ensure t
  :config
  (progn
    (window-number-meta-mode +1)
    (window-number-mode +1)))

(use-package winner
  :config (winner-mode +1))

(use-package volatile-highlights
  :ensure t)

(use-package diminish
  :ensure t
  :commands (diminish))

(use-package hl-line
  :config (global-hl-line-mode t))

(use-package fringe
  :if     (display-graphic-p)
  :config (fringe-mode '(2 . 0)))

(use-package ansi-color
  :config (ansi-color-for-comint-mode-on))

(use-package transpose-frame
  :bind (("C-x t" . transpose-frame)
         ("s-t"   . transpose-frame)
         ("C-x f" . rotate-frame)
         ("s-r"   . rotate-frame))

  :commands
  (transpose-frame
   flip-frame
   flop-frame
   rotate-frame
   rotate-frame-clockwise
   rotate-frame-anticlockwise))

(use-package key-chord
  :ensure t
  :config
  (progn
    ;; Global keys
    (key-chord-define-global "dh" 'helm-mini)
    (key-chord-define-global "x;" 'cb:kill-current-buffer)
    (key-chord-define-global "fh" 'idomenu)
    (eval-after-load 'paredit
      '(progn
         (key-chord-define paredit-mode-map "qj" 'paredit-backward-slurp-sexp)
         (key-chord-define paredit-mode-map "qk" 'cb:paredit-forward-slurp-sexp-neatly)
         (key-chord-define paredit-mode-map "ql" 'paredit-splice-sexp-killing-backward)
         (key-chord-define paredit-mode-map "qn" 'paredit-backward-barf-sexp)
         (key-chord-define paredit-mode-map "qm" 'paredit-forward-barf-sexp)))
    (key-chord-mode +1)))

(use-package cb-foundation
  :defer nil
  :bind (("s-f"     . cb:rotate-buffers)
         ("C-x C-k" . cb:kill-current-buffer)
         ("C-x C-o" . other-window))
  :config
  (defadvice cb:rotate-buffers (after select-largest-window activate)
    "Switch to the largest window if using a 2-up window configuration."
    (when (= 2 (length (window-list)))
      (cb:select-largest-window)))
  :init
  (progn
    (auto-compression-mode +1)
    (setq
     redisplay-dont-pause         t
     column-number-mode           t
     echo-keystrokes              0.02
     inhibit-startup-message      t
     transient-mark-mode          t
     shift-select-mode            nil
     require-final-newline        t
     delete-by-moving-to-trash    nil
     initial-major-mode           'emacs-lisp-mode
     initial-scratch-message      nil
     x-select-enable-clipboard    t
     font-lock-maximum-decoration t
     ring-bell-function           'ignore
     initial-scratch-message      nil
     truncate-partial-width-windows     nil
     confirm-nonexistent-file-or-buffer nil
     )
    (setq-default
     indent-tabs-mode             nil
     fill-column                  80)

    ;; Encodings
    (setq locale-coding-system   'utf-8)
    (set-terminal-coding-system  'utf-8)
    (set-keyboard-coding-system  'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system        'utf-8)

    ;; File-handling
    (add-hook 'before-save-hook 'whitespace-cleanup)
    (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style   'forward
        uniquify-separator           "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re   "^\\*"))

(use-package evil
  :ensure t
  :config
  (progn

    (use-package surround
      :ensure t
      :config (global-surround-mode +1))

    (use-package evil-numbers
      :ensure t
      :config
      (progn
        (define-key evil-normal-state-map (kbd "-") 'evil-numbers/inc-at-pt)
        (define-key evil-normal-state-map (kbd "+") 'evil-numbers/dec-at-pt)))

    (use-package cb-evil)
    (evil-mode +1)))

(use-package backup-dir
  :config
  (setq auto-save-file-name-transforms `((".*" ,(concat cb:autosaves-dir "\\1") t))
        backup-by-copying        t
        bkup-backup-directory-info `((".*" ,cb:backups-dir ok-create))
        auto-save-list-file-name (concat cb:autosaves-dir "autosave-list")
        delete-old-versions      t
        kept-new-versions        6
        kept-old-versions        2
        version-control          t))

(use-package workgroups
  :ensure t
  :diminish workgroups-mode
  :config
  (progn
    (workgroups-mode +1)
    (ignore-errors (wg-load (concat cb:etc-dir "workgroups.el")))
    (setq wg-prefix-key (kbd "C-c w"))
    (define-key wg-map [wg-prefix-key "w"] 'wg-switch-to-workgroup)))


(use-package exec-path-from-shell
  :ensure t
  :if (and (equal system-type 'darwin)
           (window-system))
  :config
  (exec-path-from-shell-initialize))

(use-package cb-osx :if (equal system-type 'darwin))

(use-package color-theme
  :config
  (setq color-theme-is-global t))

(use-package color-theme-solarized
  :ensure t
  :defer t)

(use-package ir-black-theme
  :ensure t
  :defer t)

(use-package cb-colour
  :config
  ;; Set colour by time of day.
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond
     ((not (display-graphic-p))     (solarized-light))
     ((and (<= 0 hour) (>= 6 hour)) (ir-black))
     ((or  (< 20 hour) (> 9 hour))  (solarized-dark))
     (t                             (solarized-light)))))

(use-package ediff
  :commands (ediff ediff-merge-files-with-ancestor)
  :config
  (progn
    (setq diff-switches "-u"
          ediff-window-setup-function 'ediff-setup-windows-plain)
    (add-hook 'ediff-startup-hook 'turn-off-evil-mode)))

(use-package cb-ediff
  :commands (cb:handle-git-merge))

(use-package eshell
  :commands (eshell eshell/pwd)
  :config
  (setq eshell-prompt-function
        (lambda ()
          (format "%s\n%s"
                  (abbreviate-file-name (eshell/pwd))
                  (if (= (user-uid) 0) " # " " % ")))))

(use-package shell
  :commands (shell)
  :config
  (hook-fn 'window-configuration-change-hook
    "Change process window size."
    (when (derived-mode-p 'comint-mode)
      (set-process-window-size (get-buffer-process (current-buffer))
                               (window-height)
                               (window-width)))))

(use-package auto-complete
  :ensure t
  :config
  (progn
    (use-package fuzzy
      :ensure t)
    (use-package auto-complete-config
      :config (ac-config-default))

    (setq ac-auto-show-menu t
          ac-dwim t
          ac-use-menu-map t
          ac-quick-help-delay 1
          ac-quick-help-height 60
          ac-disable-inline t
          ac-show-menu-immediately-on-auto-complete t
          ac-auto-start 2
          ac-candidate-menu-min 0
          ac-comphist-file (concat cb:tmp-dir "ac-comphist.dat"))
    (global-auto-complete-mode t)
    (ac-flyspell-workaround)
    (ac-linum-workaround)
    (define-key ac-completing-map (kbd "C-n") 'ac-next)
    (define-key ac-completing-map (kbd "C-p") 'ac-previous)
    (define-key ac-completing-map "\t" 'ac-complete)
    (define-key ac-completing-map (kbd "M-RET") 'ac-help)
    (hook-fn 'text-mode-hook
      (auto-complete-mode -1))))

(use-package cb-google
  :commands (google/search)
  :bind     (("C-c C-/" . google/search)
             ("C-c C-_" . google/search))
  :config
  (use-package w3m
    :ensure t
    :commands (w3m-browse-url)))

(use-package smartparens
  :ensure t
  :commands (smartparens-mode smartparens-global-mode)
  :init
  (defadvice smartparens-mode (around cb:inhibit-on-paredit activate)
    "Prevent smartparens from being used if paredit is active."
    (unless (and (boundp 'paredit-mode) paredit-mode)
      ad-do-it))
  :config
  (progn
    (sp-pair "'" nil :unless '(sp-point-after-word-p))
    (sp-local-tag '(sgml-mode html-mode) "<" "<_>" "</_>"
                  :transform 'sp-match-sgml-tags)
    (smartparens-global-mode +1)))

(use-package cb-indentation
  :commands (rigid-indentation-mode))

(use-package json-mode
  :ensure t
  :config
  (progn
    (add-to-list 'ac-modes 'json-mode)
    (defalias 'format-json 'beautify-json)))

(use-package lambda-mode
  :diminish lambda-mode
  :config
  (progn
    (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
    (add-hook 'inferior-lisp-mode-hook 'lambda-mode)
    (add-hook 'lisp-mode-hook          'lambda-mode)
    (add-hook 'emacs-lisp-mode-hook    'lambda-mode)
    (add-hook 'python-mode-hook        'lambda-mode)
    (add-hook 'slime-repl-mode-hook    'lambda-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"          . markdown-mode)
         ("\\.[mM]arkdown$" . markdown-mode)))

(use-package md-readme
  :config
  (progn
    (dir-locals-set-class-variables
     'generate-README-with-md-readme
     '((emacs-lisp-mode . ((mdr-generate-readme . t)))))
    (dolist (dir '("~/Projects/elisp-refactor/"))
      (dir-locals-set-directory-class
       dir 'generate-README-with-md-readme))
    (hook-fn 'after-save-hook
      (if (boundp 'mdr-generate-readme) (mdr-generate)))))

(use-package mode-compile
  :ensure t
  :bind (("C-c C-k" . mode-compile-kill)
         ("C-c C-c" . mode-compile))
  :config
  (progn
    (setq mode-compile-expert-p             t
          mode-compile-always-save-buffer-p t
          compilation-window-height         12
          compilation-scroll-output         'first-error)
    (add-to-list 'compilation-finish-functions
                 (lambda (buf str)
                   "Close compilation buffer if compilation succeeded."
                   (unless (string-match "exited abnormally" str)
                     (delete-windows-on (get-buffer-create "*compilation")))))))

(use-package flyspell-lazy
  :ensure t
  :defines flyspell-lazy-mode
  :config
  (progn
    (flyspell-lazy-mode +1)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (hook-fn 'flyspell-mode-hook
      (diminish 'flyspell-mode))))

(use-package flycheck
  :ensure t
  :config
  (let ((maybe-enable-flycheck
         (lambda ()
           (when (flycheck-may-enable-mode)
             (flycheck-mode +1)))))

    (setq flycheck-highlighting-mode 'lines)
    (add-hook 'prog-mode-hook maybe-enable-flycheck)
    (add-hook 'text-mode-hook maybe-enable-flycheck)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (setq yas/trigger-key (kbd "RET"))
    (add-to-list 'yas-snippet-dirs cb:yasnippet-dir)
    (yas--initialize)
    (yas-global-mode t)
    (hook-fn 'snippet-mode-hook
      (setq require-final-newline nil))))

(use-package eproject
  :ensure t
  :diminish eproject-minor-mode
  :config
  (hook-fn 'prog-mode-hook
    (ignore-errors (eproject-maybe-turn-on))))

(use-package make-mode
  :config
  (progn
    (add-to-list 'ac-modes 'makefile-mode)
    (hook-fn 'makefile-mode-hook
      (auto-complete-mode t)
      (setq indent-tabs-mode t))))

(use-package cb-tags
  :commands (cb:find-tag cb:load-ctags cb:build-ctags)
  :bind (("C-]"     . cb:find-tag)
         ("C-c C-r" . cb:load-ctags))
  :config
  ;; Ensure tags searches are case-sensitive.
  (setq tags-case-fold-search nil))

(use-package ctags-update
  :ensure t
  :diminish ctags-auto-update-mode
  :config
  (add-hook 'prog-mode-hook 'turn-on-ctags-auto-update-mode))

(use-package etags-select
  :ensure t
  :commands (etags-select-find-tag-at-point
             etags-select-find-tag))

(use-package cb-shebang
  :commands (insert-shebang))

(use-package nxml-mode
  :commands (nxml-mode)
  :config
  (hook-fn 'find-file-hook
    "Enable nxml-mode if this is an XML file."
    (when (or (s-ends-with? ".xml" (buffer-file-name))
              (s-starts-with? "<?xml " (buffer-string)))
      (nxml-mode)
      (local-set-key (kbd "M-q") 'cb:reformat-xml))))

(use-package tagedit
  :ensure t
  :commands (tagedit-add-paredit-like-keybindings)
  :config
  (hook-fn 'html-mode-hook
    (tagedit-add-paredit-like-keybindings)
    (setq sgml-xml-mode +1)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (define-key magit-status-mode-map (kbd "q")
      (lambda ()
        "Quit magit."
        (interactive)
        (kill-buffer)
        (jump-to-register :magit-fullscreen)))

    (hook-fn 'magit-log-edit-mode-hook
      "Enter insertion mode."
      (when (and (boundp 'evil-mode) evil-mode)
        (evil-append-line nil)))

    (add-hook 'magit-mode-hook 'magit-load-config-extensions)))

(use-package conf-mode
  :mode ((".gitignore$" . conf-mode)
         (".gitmodules$" . conf-mode)))

(use-package paredit
  :ensure t
  :commands (paredit-mode enable-paredit-mode disable-paredit-mode)
  :config
  (progn
    (use-package cb-paredit)
    (add-hook 'inferior-lisp-mode-hook 'paredit-mode)
    (add-hook 'repl-mode-hook 'paredit-mode)

    (hook-fn 'minibuffer-setup-hook
      "Use paredit in the minibuffer."
      (when (eq this-command 'eval-expression)
        (paredit-mode t)))

    (defadvice paredit-mode (after disable-smartparens activate)
      "Disable smartparens while paredit is on."
      (if ad-return-value
          (smartparens-mode -1)
        (smartparens-mode +1)))))

(use-package highlight
  :ensure t)

(use-package lively
  :ensure t)

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode)

(use-package highlight-symbol
  :ensure t)

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode)

(use-package eval-sexp-fu)

(use-package cb-lisp)

(use-package eldoc
  :commands (eldoc-mode)
  :diminish (eldoc-mode))

(use-package cb-elisp
  :config
  (progn
    (require 'ielm)
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'cb:switch-to-ielm)
    (define-key ielm-map (kbd "C-c C-z") 'cb:switch-to-elisp)))

(use-package elisp-refactor
  :bind ("M-RET" . elr-show-refactor-menu))

(use-package lisp-mode
  :commands (emacs-lisp-mode lisp-mode)
  :config
  (progn

    (hook-fn 'emacs-lisp-mode-hook
      (autoload 'ert "ert")
      (local-set-key (kbd "C-c C-t") 'ert))

    (hook-fn 'after-save-hook
      "Byte compile elisp files on save."
      (when (and (equal major-mode 'emacs-lisp-mode)
                 (buffer-file-name))
        (byte-compile-file (buffer-file-name))))))

(use-package clojure-mode
  :ensure t
  :commands (clojure-mode)
  :mode     ("\\.cljs?$" . clojure-mode)
  :config
  (progn
    (use-package cb-overtone
      :bind     ("s-." . cb:stop-overtone)
      :commands (maybe-enable-overtone-mode cb:stop-overtone))
    (use-package midje-mode
      :ensure t
      :commands (midje-mode)
      :diminish (midje-mode)
      :config   (add-hook 'clojure-mode-hook 'midje-mode))
    (use-package cb-clojure)
    (hook-fn 'clojure-mode-hook
      (maybe-enable-overtone-mode)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl))))

(use-package nrepl
  :ensure t
  :commands (nrepl-jack-in)
  :config
  (progn
    (use-package ac-nrepl)
    (setq nrepl-popup-stacktraces    nil
          nrepl-hide-special-buffers t)

    (add-to-list 'ac-modes 'nrepl-mode)
    (set-face-attribute 'nrepl-error-highlight-face t :inherit 'error)
    (set-face-underline 'nrepl-error-highlight-face nil)

    (hook-fn 'clojure-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl)
      (local-set-key (kbd "C-h f")   'nrepl-doc)
      (local-set-key (kbd "C-c C-f") 'nrepl-eval-buffer))

    (--each '(nrepl-mode-hook nrepl-interaction-mode-hook)
      (hook-fn it
        (nrepl-turn-on-eldoc-mode)
        (paredit-mode +1)
        (local-set-key (kbd "C-c l")   'nrepl-clear-buffer)
        (local-set-key (kbd "C-c C-z") 'cb:switch-to-last-clj-buffer)
        (local-set-key (kbd "C-c C-f") 'cb:eval-last-clj-buffer)))))

(use-package fsharp-mode
  :ensure t
  :commands (fsharp-mode)
  :mode ("\\.fs[ixly]?$" . fsharp-mode)
  :config
  (progn
    (add-to-list 'ac-modes 'fsharp-mode)
    (unless (display-graphic-p)
      (setq fsharp-ac-use-popup nil))
    (add-hook 'fsharp-mode-hook 'electric-indent-mode)
    (add-hook 'fsharp-mode-hook 'electric-layout-mode)))

(use-package python
  :ensure t
  :commands (python-mode)
  :config
  (progn
    (add-to-list 'ac-modes 'python-mode)
    (add-to-list 'ac-modes 'inferior-python-mode)))

(use-package ruby-mode
  :ensure t
  :modes (("\\.rake$"    . ruby-mode)
          ("Rakefile$"   . ruby-mode)
          ("\\.gemspec$" . ruby-mode))
  :defer t
  :config
  (progn

    (use-package ruby-electric
      :ensure t
      :diminish ruby-electric-mode
      :config
      (progn
        (setq ruby-electric-expand-delimiters-list '(39 96 124))
        (add-hook 'ruby-mode-hook 'ruby-electric-mode)))

    (use-package rsense
      :ensure t
      :config
      (progn
        (setq rsense-home cb:rsense-home)
        (cb:define-path cb:rsense-home "bin/rsense-0.3")
        (add-to-list 'ac-sources 'ac-source-rsense-method)
        (add-to-list 'ac-sources 'ac-source-rsense-constant)))

    (use-package inf-ruby
      :ensure t
      :config
      (progn
        (add-hook 'ruby-mode-hook 'ruby-electric-mode)
        (add-hook 'inf-ruby-mode 'inf-ruby-setup-keybindings)))

    (add-to-list 'ac-modes 'ruby-mode)
    (add-to-list 'completion-ignored-extensions ".rbc")))

(use-package haskell-mode
  :ensure t
  :commands (haskell-mode haskell-c-mode haskell-cabal-mode)
  :mode
  (("\\.hs"     . haskell-mode)
   ("\\.hsc$"   . haskell-c-mode)
   ("\\.cabal$" . haskell-cabal-mode))
  :config
  (progn

    (use-package ghc
      :ensure t)

    (use-package haskell-edit)

    (use-package haskell-indentation
      :diminish haskell-indentation-mode
      :config (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

    (use-package haskell-doc
      :diminish haskell-doc-mode
      :config (add-hook 'haskell-mode-hook 'haskell-doc-mode))

    (use-package haskell-decl-scan)

    (use-package outline
      :commands (outline-mode)
      :diminish outline-mode
      :config
      (add-hook 'haskell-mode-hook 'outline-mode))

    (use-package hs-lint
      :config
      (setq hs-lint-command (executable-find "hlint")))

    (use-package cb-haskell)

    (use-package hideshow
      :diminish hs-minor-mode)

    (add-to-list 'completion-ignored-extensions ".hi")
    (setq haskell-stylish-on-save t)

    ;; Auto-complete

    (--each '(haskell-mode
              haskell-c-mode
              haskell-cabal-mode
              haskell-interactive-mode
              inferior-haskell-mode
              )
      (add-to-list 'ac-modes it))

    (ac-define-source ghc-mod
      '((depends ghc)
        (candidates . (ghc-select-completion-symbol))
        (symbol . "s")
        (document . haskell-doc-sym-doc)
        (cache)))

    (hook-fn 'haskell-mode-hook
      (setq evil-shift-width     4
            tab-width            4
            haskell-tags-on-save t)
      (ignore-errors (paredit-mode +1))
      ;; Set key bindings.
      (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build)
      (local-set-key (kbd "C-c h")   'hoogle)
      (local-set-key (kbd "C-c l")   'hs-lint)
      (local-set-key (kbd "C-c j")   'haskell-test<->code)
      ;; Configure outlining.
      (setq outline-regexp cb:haskell-outline-regex
            outline-level 'cb:hs-outline-level)
      (outline-minor-mode t)
      ;; Configure auto-complete sources.
      (setq ac-sources (list 'ac-source-words-in-same-mode-buffers
                             'ac-source-ghc-mod)))))

;;; ----------------------------------------------------------------------------
;;; Error navigation keybindings.

(global-set-key (kbd "M-N") 'next-error)
(global-set-key (kbd "M-P") 'previous-error)

(defun cb:byte-compile-conf ()
  "Recompile all configuration files."
  (interactive)
  (byte-recompile-file (concat user-emacs-directory "init.el") t 0)
  (byte-recompile-directory cb:lib-dir 0 t)
  (byte-recompile-directory cb:lisp-dir 0 t))

(defun cb:byte-compile-elpa ()
  "Recompile all lisp files in `user-emacs-directory'."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0 t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((mdr-generate-readme . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:

;;; init.el ends here
