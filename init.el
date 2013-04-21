;;; init --- My emacs configuration.

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

;;; Disable intrusive GUI elements.

(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(blink-cursor-mode -1)

;; Use the menu-bar only in OS X Aqua.
(if (and (display-graphic-p) (equal system-type 'darwin))
    (menu-bar-mode +1)
  (menu-bar-mode -1))

;;; Fully-qualify `user-emacs-directory' and peform essential loads.
(setq user-emacs-directory (expand-file-name user-emacs-directory))
(require 'bind-key (concat user-emacs-directory "lib/use-package/bind-key.el"))
(require 'use-package (concat user-emacs-directory "lib/use-package/use-package.el"))

;;; Describe me.

(setq
 user-full-name    "Chris Barrett"
 user-mail-address "chris.d.barrett@me.com")

;;; Basic configuration.

(setq
 redisplay-dont-pause         t
 column-number-mode           t
 echo-keystrokes              0.02
 inhibit-startup-message      t
 transient-mark-mode          t
 shift-select-mode            nil
 require-final-newline        t
 delete-by-moving-to-trash    nil
 initial-major-mode           'text-mode
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
(icomplete-mode +1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Encodings

(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

;; File-handling

(auto-compression-mode +1)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Set exec-path manually in OS X.

(when (and (display-graphic-p) (equal system-type 'darwin))
  (let ((home (getenv "HOME")))
    (add-to-list 'exec-path (concat home "/.carton/bin"))
    (add-to-list 'exec-path (concat home "/bin"))
    (add-to-list 'exec-path (concat home "/.cabal/bin"))
    (add-to-list 'exec-path "/usr/local/bin")
    (add-to-list 'exec-path "/opt/local/bin")
    (add-to-list 'exec-path "/opt/local/sbin"))

  (setenv "PATH" (mapconcat 'identity exec-path ":")))

;;; Help commands

(define-prefix-command 'help-find-map)
(bind-key (kbd "C-h e") 'help-find-map)
(bind-key (kbd "C-h e e") 'view-echo-area-messages)
(bind-key (kbd "C-h e f") 'find-function)
(bind-key (kbd "C-h e k") 'find-function-on-key)
(bind-key (kbd "C-h e l") 'find-library)
(bind-key (kbd "C-h e p") 'find-library)
(bind-key (kbd "C-h e v") 'find-variable)
(bind-key (kbd "C-h e V") 'apropos-value)

;;; Disable vc modes

(setq vc-handled-backends nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(eval-after-load "vc"
  '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;;; Editing Advice

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Suppress \"Active processes exist\" query when exiting Emacs."
  (flet ((process-list ()))
    ad-do-it))

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Trim whitespace on `kill-line'."
  (unless (bolp)
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  "Fix `whitespace-cleanup' bug when using `indent-tabs-mode'."
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

;;; Byte compilation commands

(defun cb:byte-compile-conf ()
  "Recompile all configuration files."
  (interactive)
  (byte-recompile-file (concat user-emacs-directory "init.el") t 0)
  (byte-recompile-directory cb:lib-dir 0 t)
  (byte-recompile-directory cb:lisp-dir 0 t))

(defun cb:byte-compile-elpa ()
  "Recompile all lisp files in the package directory."
  (interactive)
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0 t))

;;; ----------------------------------------------------------------------------
;;; Initialize packages.

(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; ============================================================================
;;; Load packages.

(use-package cl-lib)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package cb-macros
  :init
  (progn
    (cb:define-path cb:lib-dir       "lib/" t)
    (cb:define-path cb:lisp-dir      "lisp/" t)
    (cb:define-path cb:src-dir       "src")
    (cb:define-path cb:tmp-dir       "tmp/")
    (cb:define-path cb:elpa-dir      "elpa/")
    (cb:define-path cb:bin-dir       "bin/")
    (cb:define-path cb:etc-dir       "etc/")
    (cb:define-path cb:yasnippet-dir "snippets/")
    (cb:define-path cb:backups-dir   "backups/")
    (cb:define-path cb:autosaves-dir "tmp/autosaves/")
    (cb:define-path cb:rsense-home   "bin/rsense-0.3/")

    ;; Use the version of emacs in /src for info and source.
    (setq source-directory (format "%s/emacs-%s.%s" cb:src-dir
                                   emacs-major-version
                                   emacs-minor-version))
    (setenv "INFOPATH" (concat source-directory "/info/"))))

;;; ----------------------------------------------------------------------------
;;; Combined hooks.
;;;
;;; These hooks are used to configure similar modes that do not have derivation
;;; relationships. e.g. language families.

(defmacro define-combined-hook (name modes)
  "Define a hook named NAME to be run after each of the setup hooks for MODES."
  `(progn
     ;; Define hook variable.
     (defvar ,name)
     ;; Add hook for each mode.
     (--each ,modes
       (hook-fn (intern (concat (symbol-name it) "-hook"))
         (run-hooks ',name)))
     ',name))

;;; Lisp hooks

(defvar cb:lisp-modes
  '(scheme-mode
    emacs-lisp-mode
    lisp-mode
    common-lisp-mode
    repl-mode
    clojure-mode
    clojurescript-mode
    ielm-mode))

(define-combined-hook cb:lisp-shared-hook cb:lisp-modes)

;;; Haskell hooks.

(define-combined-hook cb:haskell-shared-hook
  '(haskell-mode
    inferior-haskell-mode
    haskell-interactive-mode
    haskell-c-mode
    haskell-cabal-mode))

;;; ----------------------------------------------------------------------------

(hook-fn 'prog-mode-hook
  "Generic programming mode configuration."

  ;; Error navigation keybindings.
  (local-set-key (kbd "M-N") 'next-error)
  (local-set-key (kbd "M-P") 'previous-error)

  ;; Highlight special comments.
  (font-lock-add-keywords
   major-mode '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                 1 font-lock-warning-face t))))

(hook-fn 'Buffer-menu-mode-hook
  "Buffer menu only shows files on disk."
  (Buffer-menu-toggle-files-only +1))

;;; Forward-declare ac-modes so auto-complete can be safely loaded separately
;;; from other modes.
(defvar ac-modes nil)

;;; ----------------------------------------------------------------------------
;;; Mail configuration

(setq mail-signature (concat "\nCheers,\n\n" user-full-name)
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(use-package smtpmail
  :commands smtpmail-send-it
  :init
  (setq
   smtpmail-mail-address user-mail-address
   smtpmail-smtp-server "smtp.mail.me.com"
   smtpmail-smtp-service 587))

(use-package gnus
  :commands gnus
  :config
  (setq
   gnus-select-method '(nnml "mail")
   gnus-secondary-select-methods
   `((nnimap "mail"
             (nnimap-address ,smtpmail-smtp-server)
             (nnimap-list-pattern ("INBOX" "mail/*"))
             (nnimap-server-port 587)
             (nnimap-stream ssl)
             (nnimap-authenticator login)))))

;;; ----------------------------------------------------------------------------

(use-package helm
  :ensure t
  :defer  t)

(use-package imenu
  :config
  (hook-fn 'emacs-lisp-mode-hook
    "Display section headings."
    (setq imenu-prev-index-position-function nil)
    (add-to-list 'imenu-generic-expression
                 `("SECTION"
                   ;; Match sections.
                   ,(rx bol ";;;" (* space) (+ "-") (? "\n")
                        ";;;" (* space)
                        (group (1+ nonl))
                        (* "-") eol) 1) t)))

(use-package ido
  :ensure t
  :defer  nil
  :init
  (setq ido-enable-prefix            nil
        ido-save-directory-list-file (concat cb:tmp-dir "ido.last")
        ido-enable-flex-matching     t
        ido-create-new-buffer        'always
        ido-use-filename-at-point    'guess
        ido-max-prospects            10
        ido-default-file-method      'selected-window)
  :config
  (progn
    (ido-mode +1)
    (add-to-list 'ido-ignore-buffers "*helm mini*")
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package ido-hacks
  :ensure t
  :config (ido-hacks-mode +1))

(use-package ido-ubiquitous
  :ensure t
  :config (ido-ubiquitous-mode +1))

(use-package ido-yes-or-no
  :ensure t
  :config (ido-yes-or-no-mode +1))

(use-package ido-better-flex
  :ensure t
  :config (ido-better-flex/enable))

(use-package ido-speed-hack)

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind   (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)))

(use-package popwin
  :ensure t
  :defer nil
  :config
  (progn
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
            ("*sldb.*":regexp t :height 30)))

    (popwin-mode +1)))

(use-package saveplace
  :init
  (progn
    (setq save-place-file (concat cb:tmp-dir "saved-places"))
    (setq-default save-place t)))

(use-package paren
  :config (show-paren-mode +1))

(use-package recentf
  :init
  (progn
    (setq
     recentf-save-file       (concat cb:tmp-dir "recentf")
     recentf-keep            '(file-remote-p file-readable-p)
     recentf-max-saved-items 100
     recentf-max-menu-items  25

     recentf-exclude
     '(".newsrc" "-autoloads.el" "recentf" ".ido.last" "TAGS" ".gz"))

    (recentf-mode +1)))

(use-package savehist
  :init
  (progn
    (setq
     savehist-additional-variables '(search ring regexp-search-ring)
     savehist-autosave-interval    60
     savehist-file                 (concat cb:tmp-dir "savehist"))
    (savehist-mode +1)))

(use-package undo-tree
  :ensure   t
  :diminish undo-tree-mode
  :init    (global-undo-tree-mode +1))

(use-package window-number
  :ensure t
  :config (window-number-meta-mode +1))

(use-package windmove
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)))

(use-package winner
  :config (winner-mode +1))

(use-package diminish
  :ensure t
  :commands diminish)

(use-package hl-line
  :if (display-graphic-p)
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
    ;; Global keys.
    (key-chord-define-global "dh" 'helm-mini)
    (key-chord-define-global "x;" 'cb:kill-current-buffer)
    (key-chord-define-global "fh" 'helm-imenu)

    ;; Paredit keys.
    (eval-after-load 'paredit
      '(progn
         (key-chord-define paredit-mode-map "qj" 'paredit-backward-slurp-sexp)
         (key-chord-define paredit-mode-map "qk" 'cb:paredit-forward-slurp-sexp-neatly)
         (key-chord-define paredit-mode-map "ql" 'paredit-splice-sexp-killing-backward)
         (key-chord-define paredit-mode-map "qn" 'paredit-backward-barf-sexp)
         (key-chord-define paredit-mode-map "qm" 'paredit-forward-barf-sexp)))

    (key-chord-mode +1)))

(use-package cb-commands
  :bind (("s-f"     . cb:rotate-buffers)
         ("C-x C-o" . other-window))

  :commands (cb:hide-dos-eol
             cb:kill-current-buffer
             insert-timestamp
             cb:last-buffer-for-mode)

  :init     (add-hook 'find-file-hook 'cb:hide-dos-eol)

  :config
  (defadvice cb:rotate-buffers (after select-largest-window activate)
    "Switch to the largest window if using a 2-up window configuration."
    (when (= 2 (length (window-list)))
      (cb:select-largest-window))))

(use-package scratch
  :ensure t
  :commands (scratch))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style   'forward
        uniquify-separator           "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re   "^\\*"))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-L" . ace-jump-line-mode)
         ("C-SPC" . ace-jump-word-mode)
         ;; Needed for terminal.
         ("C-@" . ace-jump-word-mode))
  :config
  (progn
    (add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

    ;; Use ESC to quit ace-jump.
    (--each '(ace-jump-line-mode ace-jump-word-mode ace-jump-char-mode)
      (hook-fn it (local-set-key (kbd "ESC") 'keyboard-quit)))))

(use-package hideshow
  :diminish hs-minor-mode)

(use-package evil
  :ensure t
  :commands evil-mode

  :init
  (progn
    (evil-mode +1)
    (hook-fn 'comint-mode-hook 'evil-append-line))

  :config
  (progn
    (require 'cb-evil)

    (defun cb:evil-undefine ()
      "Temporarily undefine a key for Evil minor mode."
      (interactive)
      (let ((evil-mode-map-alist))
        (call-interactively (key-binding (this-command-keys)))))

    (define-key evil-normal-state-map (kbd "M-z") 'evil-emacs-state)
    (define-key evil-emacs-state-map  (kbd "M-z") 'evil-normal-state)
    (define-key evil-normal-state-map (kbd "C-z") 'cb:evil-undefine)
    (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
    (define-key evil-insert-state-map (kbd "C-z") 'cb:evil-undefine)
    (define-key evil-visual-state-map (kbd "C-z") 'cb:evil-undefine)

    ;; Ensure undo-tree commands are remapped. The referenced keymap in
    ;; evil-integration is incorrect.
    (define-key undo-tree-visualizer-mode-map [remap evil-backward-char]
      'undo-tree-visualize-switch-branch-left)
    (define-key undo-tree-visualizer-mode-map [remap evil-forward-char]
      'undo-tree-visualize-switch-branch-right)
    (define-key undo-tree-visualizer-mode-map [remap evil-next-line]
      'undo-tree-visualize-redo)
    (define-key undo-tree-visualizer-mode-map [remap evil-previous-line]
      'undo-tree-visualize-undo)

    ;; Use ESC as quit command in most situations.
    (--each '(evil-normal-state-map
              evil-visual-state-map
              minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
      (define-key (eval it) [escape] 'keyboard-quit))

    (setq evil-want-visual-char-semi-exclusive t
          evil-toggle-key (kbd "M-z")
          evil-default-cursor t)
    (setq-default evil-shift-width 2)))

(use-package evil-paredit
  :ensure t
  :commands evil-paredit-mode
  :config (add-hook 'paredit-mode-hook 'evil-paredit-mode))

(use-package surround
  :ensure t
  :config (global-surround-mode +1))

(use-package evil-numbers
  :ensure t
  :commands (evil-numbers/dec-at-pt evil-numbers/inc-at-pt)
  :config
  (progn
    (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
    (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)))

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

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package cb-osx :if (equal system-type 'darwin))

(use-package color-theme
  :config (setq color-theme-is-global t))

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package ir-black-theme
  :ensure t
  :defer t)

(use-package cb-colour
  :if (display-graphic-p)
  :config
  ;; Set colour by time of day.
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond
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
  :commands cb:handle-git-merge)

(use-package eshell
  :commands eshell
  :config
  (setq eshell-prompt-function
        (lambda ()
          (format "%s\n%s"
                  (abbreviate-file-name (eshell/pwd))
                  (if (= (user-uid) 0) " # " " % ")))))

(use-package shell
  :commands shell
  :config
  (hook-fn 'window-configuration-change-hook
    "Change process window size."
    (when (derived-mode-p 'comint-mode)
      (set-process-window-size (get-buffer-process (current-buffer))
                               (window-height)
                               (window-width)))))

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :commands (global-auto-complete-mode auto-complete-mode)

  :init
  (progn
    (add-hook 'prog-mode-hook 'auto-complete-mode)
    (add-hook 'comint-mode-hook 'auto-complete-mode))

  :config
  (progn

    ;; Enable for everything except text modes.
    (global-auto-complete-mode +1)
    (hook-fn 'text-mode-hook
      (auto-complete-mode -1))

    (use-package auto-complete-config
      :config (ac-config-default))

    (--each cb:lisp-modes (add-to-list 'ac-modes it))
    (setq
     ac-auto-show-menu t
     ac-dwim t
     ac-use-menu-map t
     ac-quick-help-delay 1
     ac-quick-help-height 60
     ac-disable-inline t
     ac-show-menu-immediately-on-auto-complete t
     ac-auto-start 2
     ac-candidate-menu-min 0
     ac-comphist-file (concat cb:tmp-dir "ac-comphist.dat"))

    (ac-flyspell-workaround)

    (define-key ac-completing-map (kbd "C-n") 'ac-next)
    (define-key ac-completing-map (kbd "C-p") 'ac-previous)
    (define-key ac-completing-map "\t" 'ac-complete)
    (define-key ac-completing-map (kbd "M-RET") 'ac-help)))

(use-package fuzzy
  :ensure t)

(use-package cb-google
  :commands google/search
  :bind     (("C-c C-/" . google/search)
             ("C-c C-_" . google/search)))

(use-package w3m
  :ensure t
  :commands (w3m-find-file w3m-browse-url))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands (smartparens-mode smartparens-global-mode)
  :init
  (progn
    (add-hook 'text-mode-hook 'smartparens-mode)
    (add-hook 'comint-mode-hook 'smartparens-mode)
    ;; Don't enable smartparents mode for lisps. Make doubly-sure Paredit is
    ;; used.
    (hook-fn 'prog-mode-hook
      (if (s-matches? (rx (or "lisp" "clojure")) (symbol-name major-mode))
          (paredit-mode +1)
        (smartparens-mode +1))))
  :config
  (progn
    (sp-pair "'" nil :unless '(sp-point-after-word-p))
    (sp-local-tag '(sgml-mode html-mode) "<" "<_>" "</_>"
                  :transform 'sp-match-sgml-tags)))

(use-package smart-operator
  :ensure t
  :commands (smart-insert-operator smart-insert-operator-hook)
  :init
  (progn

    (defun cb:python-smart-equals ()
      "Insert an '=' char padded by spaces, except in function arglists."
      (interactive)
      (if (string-match-p
           (rx (* space) "def ")
           (buffer-substring (line-beginning-position) (line-end-position)))
          (insert-string "=")
        (smart-insert-operator "=")))

    (hook-fn 'python-mode-hook
      (smart-insert-operator-hook)
      (local-set-key (kbd "=") 'cb:python-smart-equals)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'cb:haskell-shared-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd ":"))
      (local-unset-key (kbd ".")))

    (hook-fn 'sclang-mode-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd ".")))

    (hook-fn 'inferior-python-mode-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'c-mode-common-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":"))
      (local-set-key (kbd "*") 'c-electric-star))))

(use-package json-mode
  :ensure t
  :commands json-mode
  :mode ("\\.json$" . json-mode)
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
         ("\\.[mM]arkdown$" . markdown-mode))
  :config
  (hook-fn 'markdown-mode-hook
    (setq imenu-generic-expression
          '(("title"  "^\\(.*\\)[\n]=+$" 1)
            ("h2-"    "^\\(.*\\)[\n]-+$" 1)
            ("h1"   "^# \\(.*\\)$" 1)
            ("h2"   "^## \\(.*\\)$" 1)
            ("h3"   "^### \\(.*\\)$" 1)
            ("h4"   "^#### \\(.*\\)$" 1)
            ("h5"   "^##### \\(.*\\)$" 1)
            ("h6"   "^###### \\(.*\\)$" 1)
            ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))))

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
    (setq ispell-dictionary "english")
    (flyspell-lazy-mode +1)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (hook-fn 'flyspell-mode-hook
      (diminish 'flyspell-mode))))

(use-package flycheck
  :ensure t
  :commands (flycheck-may-enable-mode flycheck-mode)
  :init
  (let ((maybe-enable-flycheck
         (lambda ()
           "Do not enable flycheck for /src and /elpa."
           (when (and (flycheck-may-enable-mode))
             (unless (or (s-contains? cb:elpa-dir (or (buffer-file-name) ""))
                         (s-contains? cb:src-dir  (or (buffer-file-name) "")))
               (flycheck-mode +1))))))

    (setq flycheck-highlighting-mode 'lines)
    (add-hook 'text-mode-hook maybe-enable-flycheck)
    (add-hook 'prog-mode-hook maybe-enable-flycheck)))

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode)
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (progn
    (setq yas/trigger-key (kbd "RET"))
    (add-to-list 'yas-snippet-dirs cb:yasnippet-dir)
    (yas/global-mode t)
    (hook-fn 'snippet-mode-hook
      (setq require-final-newline nil))))

(use-package eproject
  :ensure t
  :commands eproject-maybe-turn-on
  :diminish eproject-mode
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
  :commands cb:build-ctags
  :bind (("C-]"     . cb:find-tag)
         ("C-c C-r" . cb:load-ctags))
  :config
  (progn
    ;; Ensure tags searches are case-sensitive.
    (setq tags-case-fold-search nil)
    (global-set-key (kbd "M-.") 'find-tag)))

(use-package ctags-update
  :ensure   t
  :diminish ctags-auto-update-mode
  :config   (add-hook 'prog-mode-hook 'turn-on-ctags-auto-update-mode))

(use-package etags-select
  :ensure   t
  :commands (etags-select-find-tag-at-point etags-select-find-tag))

(use-package cb-shebang
  :commands insert-shebang)

(use-package nxml-mode
  :commands nxml-mode
  :config
  (hook-fn 'find-file-hook
    "Enable nxml-mode if this is an XML file."
    (when (or (s-ends-with? ".xml" (buffer-file-name))
              (s-starts-with? "<?xml " (buffer-string)))
      (nxml-mode)
      (local-set-key (kbd "M-q") 'cb:reformat-xml))))

(use-package tagedit
  :ensure   t
  :commands (tagedit-add-paredit-like-keybindings)
  :config
  (hook-fn 'html-mode-hook
    (tagedit-add-paredit-like-keybindings)
    (setq sgml-xml-mode +1)))

(use-package magit
  :ensure t
  :bind   ("C-x g" . magit-status)
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
  :ensure   t
  :diminish paredit-mode
  :commands (paredit-mode enable-paredit-mode disable-paredit-mode)
  :init
  (progn

    (hook-fn 'minibuffer-setup-hook
      "Use paredit in the minibuffer."
      (when (eq this-command 'eval-expression)
        (paredit-mode t)))

    (hook-fn 'paredit-mode-hook
      "Turn off smart parens."
      (when (featurep 'smartparens)
        (turn-off-smartparens-mode))))

  :config
  (progn
    (use-package cb-paredit)
    (add-hook 'cb:lisp-shared-hook 'enable-paredit-mode)
    (add-hook 'inferior-lisp-mode-hook 'paredit-mode)
    (add-hook 'repl-mode-hook 'paredit-mode)))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

(use-package highlight-symbol
  :ensure   t
  :diminish highlight-symbol-mode
  :commands highlight-symbol-mode
  :init     (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config   (setq highlight-symbol-idle-delay 0.5))

(use-package parenface-plus
  :ensure t)

(use-package eval-sexp-fu
  :commands eval-sexp-fu-flash-mode
  :init     (add-hook 'cb:lisp-shared-hook 'eval-sexp-fu-flash-mode)
  :config   (setq eval-sexp-fu-flash-duration 0.2))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (progn
    (add-hook 'cb:lisp-shared-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package c-eldoc
  :ensure   t
  :commands c-turn-on-eldoc-mode
  :init     (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(use-package cb-elisp
  :commands (cb:switch-to-ielm cb:switch-to-elisp)
  :defer nil

  :init
  (progn
    (autoload 'ert-modeline-mode "ert-modeline")
    (add-hook 'emacs-lisp-mode-hook 'ert-modeline-mode)
    (add-hook 'after-save-hook 'check-parens)
    (add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))

    (bind-key (kbd "C-c e b") 'eval-buffer)
    (bind-key (kbd "C-c e e") 'toggle-debug-on-error)
    (bind-key (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
    (bind-key (kbd "C-c e r") 'eval-region)
    (bind-key (kbd "C-c e s") 'scratch)

    ;; Switching back-and-forth from IELM.
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'cb:switch-to-ielm)
    (hook-fn 'ielm-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-elisp))

    (hook-fn 'emacs-lisp-mode-hook
      (require 'cb-elisp)))

  :config
  (progn
    (defadvice eval-buffer (before buffer-evaluated-feedback activate)
      "Message that the buffer has been evaluated.
This has to be BEFORE advice because `eval-buffer' doesn't return anything."
      (when (buffer-file-name)
        (message "Buffer evaluated.")))))

(use-package redshank
  :ensure   t
  :commands turn-on-redshank-mode
  :diminish redshank-mode
  :init    (add-hook 'emacs-lisp-mode-hook 'turn-on-redshank-mode))

(use-package macrostep
  :ensure t
  :bind   ("C-c e m" . macrostep-expand)
  :config (evil-add-hjkl-bindings macrostep-mode-map 'motion))

(use-package elisp-slime-nav
  :ensure   t
  :diminish elisp-slime-nav-mode
  :init
  (--each '(emacs-lisp-mode-hook ielm-mode-hook)
    (hook-fn it
      (elisp-slime-nav-mode +1)
      (local-set-key (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

      ;; Make M-. work in normal state.
      (when (featurep 'evil)
        (define-key evil-normal-state-map (kbd "M-.")
          'elisp-slime-nav-find-elisp-thing-at-point)))))

(use-package litable
  :ensure   t
  :commands litable-mode
  :init     (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'litable-mode))

(use-package emr
  :bind   ("M-RET" . emr-show-refactor-menu)
  :config (require 'emr-elisp))

(use-package lisp-mode
  :commands (emacs-lisp-mode lisp-mode)
  :config
  (progn

    (hook-fn 'emacs-lisp-mode-hook
      (autoload 'ert "ert")
      (local-set-key (kbd "C-c C-t") 'ert))

    (hook-fn 'after-save-hook
      "Byte compile elisp source files on save."
      (when (and (equal major-mode 'emacs-lisp-mode)
                 (buffer-file-name)
                 (not (equal "Carton" (file-name-nondirectory (buffer-file-name)))))
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
      :ensure   t
      :commands midje-mode
      :diminish midje-mode
      :init    (add-hook 'clojure-mode-hook 'midje-mode))

    (defun cb:switch-to-nrepl ()
      "Start nrepl or switch to an existing nrepl buffer."
      (interactive)
      (if-let (buf (get-buffer "*nrepl*"))
        (nrepl-switch-to-repl-buffer buf)
        (nrepl-jack-in)))

    (hook-fn 'clojure-mode-hook
      (maybe-enable-overtone-mode)
      (subword-mode +1)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl))))

(use-package nrepl
  :ensure   t
  :commands nrepl-jack-in
  :config
  (progn

    (defadvice nrepl-switch-to-repl-buffer (after insert-at-end-of-nrepl-line activate)
      "Enter insertion mode at the end of the line when switching to nrepl."
      (when (and (boundp 'evil-mode) evil-mode)
        (evil-goto-line)
        (evil-append-line 0)))

    (defadvice back-to-indentation (around move-to-nrepl-bol activate)
      "Move to position after prompt in nREPL."
      (if (equal major-mode 'nrepl-mode)
          (nrepl-bol)
        ad-do-it))

    (use-package ac-nrepl
      :ensure t
      :commands (ac-nrepl-setup ac-nrepl-doc)
      :init
      (progn
        (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
        (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
        (add-to-list 'ac-modes 'nrepl-mode))
      :config
      (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

    (defun cb:switch-to-clojure ()
      "Switch to the last active clojure buffer."
      (interactive)
      (when-let (buf (cb:last-buffer-for-mode 'clojure-mode))
        (pop-to-buffer buf)))

    (defun cb:eval-last-clj-buffer ()
      "Evaluate that last active clojure buffer without leaving the repl."
      (interactive)
      (when-let (buf (cb:last-buffer-for-mode 'clojure-mode))
        (with-current-buffer buf
          (nrepl-eval-buffer))))

    (setq
     nrepl-popup-stacktraces    nil
     nrepl-hide-special-buffers t)

    (set-face-attribute 'nrepl-error-highlight-face t :inherit 'error)
    (set-face-underline 'nrepl-error-highlight-face nil)

    (hook-fn 'clojure-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl)
      (local-set-key (kbd "C-h f")   'nrepl-doc)
      (local-set-key (kbd "C-c C-f") 'nrepl-eval-buffer))

    (--each '(nrepl-mode-hook nrepl-interaction-mode-hook)
      (hook-fn it
        (nrepl-turn-on-eldoc-mode)
        (subword-mode +1)
        (paredit-mode +1)
        (local-set-key (kbd "C-c l")   'nrepl-clear-buffer)
        (local-set-key (kbd "C-c C-z") 'cb:switch-to-clojure)
        (local-set-key (kbd "C-c C-f") 'cb:eval-last-clj-buffer)))))

(use-package sclang
  :commands (sclang-mode sclang-start)
  :mode ("\\.sc$" . sclang-mode)
  :config
  (progn
    (setq sclang-auto-scroll-post-buffer t
          sclang-eval-line-forward nil)

    ;; Configure paths.
    (let* ((bundle "/Applications/SuperCollider/SuperCollider.app")
           (app-resources (concat bundle "/Contents/Resources"))
           (help-path     (concat app-resources "/HelpSource")))
      (setq
       sclang-runtime-directory (concat app-resources "/SCClassLibrary")

       sclang-program (concat app-resources "/sclang")

       sclang-extension-path
       (list "~/Library/Application Support/SuperCollider/Extensions")

       sclang-help-path (list help-path (concat help-path "/Classes"))))

    (hook-fn 'sclang-mode-hook
      (local-set-key (kbd "s-.") 'sclang-main-stop)
      (smartparens-mode +1)
      (unless (sclang-server-running-p)
        (sclang-server-boot)))))

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

(use-package python
  :ensure   t
  :commands python-mode
  :mode     ("\\.py$" . python-mode)
  :config
  (progn

    (use-package jedi
      :ensure   t
      :commands jedi:setup
      :init
      (progn
        (setq jedi:setup-keys t)
        (add-hook 'inferior-python-mode-hook 'jedi:setup)
        (add-hook 'python-mode-hook 'jedi:setup)))

    (defun cb:comma-then-space ()
      (interactive)
      (atomic-change-group
        (insert-char ?\,)
        (just-one-space)))

    (defun cb:switch-to-python ()
      "Switch to the last active Python buffer."
      (interactive)
      (when-let (buf (cb:last-buffer-for-mode 'python-mode))
        (pop-to-buffer buf)))

    (define-key python-mode-map (kbd ",") 'cb:comma-then-space)
    (define-key inferior-python-mode-map (kbd ",") 'cb:comma-then-space)
    (define-key inferior-python-mode-map (kbd "C-c C-z") 'cb:switch-to-python)
    (add-to-list 'ac-modes 'python-mode)
    (add-to-list 'ac-modes 'inferior-python-mode)))

(use-package ruby-mode
  :ensure t
  :mode (("\\.rake\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode)
         ("Thorfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode))
  :config
  (progn

    (use-package rsense
      :ensure t
      :config
      (progn
        (setq rsense-home cb:rsense-home)
        (cb:define-path cb:rsense-home "bin/rsense-0.3")

        (hook-fn 'inf-ruby-mode-hook
          (add-to-list 'ac-sources 'ac-source-rsense-method)
          (add-to-list 'ac-sources 'ac-source-rsense-constant))

        (hook-fn 'ruby-mode-hook
          (add-to-list 'ac-sources 'ac-source-rsense-method)
          (add-to-list 'ac-sources 'ac-source-rsense-constant))))

    (use-package yari
      :ensure t
      :commands yari
      :init
      (progn
        (hook-fn 'ruby-mode-hook
          (local-set-key (kbd "C-c C-h") 'yari))

        (hook-fn 'inf-ruby-mode-hook
          (local-set-key (kbd "C-c C-h") 'yari))))

    (use-package inf-ruby
      :ensure   t
      :commands inf-ruby-mode
      :config
      (hook-fn 'inf-ruby-mode
        (ruby-electric-mode -1)
        (subword-mode +1)
        ;; Stop IRB from echoing input.
        (setq comint-process-echoes t)
        (inf-ruby-setup-keybindings)))

    (use-package ruby-tools
      :ensure   t
      :diminish ruby-tools-mode
      :commands ruby-tools-mode
      :init     (add-hook 'ruby-mode-hook 'ruby-tools-mode))

    (use-package ruby-end
      :ensure   t
      :diminish ruby-end-mode
      :commands ruby-end-mode
      :init     (add-hook 'ruby-mode-hook 'ruby-end-mode))

    (add-to-list 'ac-modes 'ruby-mode)
    (add-to-list 'completion-ignored-extensions ".rbc")

    (hook-fn 'ruby-mode-hook
      (ruby-electric-mode -1)
      (subword-mode +1))))

(use-package yaml-mode
  :ensure   t
  :commands yaml-mode
  :mode     (("\\.yaml$" . yaml-mode)
             ("\\.yml$"  . yaml-mode)))

(use-package haskell-mode
  :ensure t
  :commands (haskell-mode haskell-c-mode haskell-cabal-mode hoogle)
  :mode
  (("\\.hs$"    . haskell-mode)
   ("\\.hsc$"   . haskell-c-mode)
   ("\\.cabal$" . haskell-cabal-mode))
  :config
  (progn

    (use-package cb-haskell
      :init
      (hook-fn 'haskell-mode-hook
        (require 'cb-haskell)
        (local-set-key (kbd "C-c j") 'haskell-test<->code)))

    (use-package ghc
      :ensure   t
      :commands ghc-init
      :init     (add-hook 'haskell-mode-hook 'ghc-init)
      :config
      (progn

        (defun ac-haskell-candidates ()
          "Auto-complete source using ghc-doc."
          (let ((pat (buffer-substring (ghc-completion-start-point) (point))))
            (all-completions pat (ghc-select-completion-symbol))))

        (ac-define-source ghc
          '((candidates . ac-haskell-candidates)))

        (hook-fn 'haskell-mode-hook
          (add-to-list 'ac-sources 'ac-source-ghc))))

    (use-package haskell-ac
      :init
      (hook-fn 'cb:haskell-shared-hook
        (add-to-list 'ac-modes major-mode)
        (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
        (add-to-list 'ac-sources 'ac-source-haskell)))

    (use-package haskell-edit
      :commands (haskell-find-type-signature
                 haskell-reformat-type-signature))

    (use-package haskell-indentation
      :diminish haskell-indentation-mode
      :config   (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

    (use-package haskell-doc
      :diminish haskell-doc-mode
      :commands haskell-doc-mode
      :init     (add-hook 'cb:haskell-shared-hook 'haskell-doc-mode))

    (use-package haskell-decl-scan
      :commands turn-on-haskell-decl-scan
      :init     (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan))

    (use-package hs-lint
      :commands hs-lint
      :config
      (progn
        (setq hs-lint-command (executable-find "hlint"))
        (hook-fn 'haskell-mode-hook
          (local-set-key (kbd "C-c l") 'hs-lint))))

    (defadvice switch-to-haskell (after insert-at-end-of-line activate)
      "Enter insertion mode at the end of the line when switching to inferior haskell."
      (when (and (boundp 'evil-mode) evil-mode)
        (evil-goto-line)
        (evil-append-line 0)))

    (add-to-list 'completion-ignored-extensions ".hi")

    (hook-fn 'cb:haskell-shared-hook
      (subword-mode +1)
      (local-set-key (kbd "C-c h") 'hoogle))

    (defun cb:switch-to-haskell ()
      "Switch to the last active Haskell buffer."
      (interactive)
      (when-let (buf (cb:last-buffer-for-mode 'haskell-mode))
        (pop-to-buffer buf)))

    (hook-fn 'inferior-haskell-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-haskell))

    (hook-fn 'haskell-mode-hook
      (setq
       evil-shift-width     4
       tab-width            4
       haskell-tags-on-save t
       haskell-stylish-on-save t)
      (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build))))

(use-package workgroups
  :if       (display-graphic-p)
  :bind     (("s-1" . wg-switch-to-index-0)
             ("s-2" . wg-switch-to-index-1)
             ("s-3" . wg-switch-to-index-2))
  :ensure   t
  :defer    nil
  :commands workgroups-mode
  :diminish workgroups-mode
  :config
  (progn
    (set-face-foreground 'wg-divider-face "light slate grey")
    (set-face-foreground 'wg-mode-line-face "light slate grey")
    (ignore-errors (wg-load (concat cb:etc-dir "workgroups")))
    (setq wg-prefix-key (kbd "C-c w"))

    (workgroups-mode +1)))

(use-package org
  :ensure t
  :defer t
  :config
  (progn
    (define-key org-mode-map (kbd "M-p") 'org-metaup)
    (define-key org-mode-map (kbd "M-n") 'org-metadown)))

;;; ----------------------------------------------------------------------------
;;; Show quote if 'fortune' is installed.

(defun fortune ()
  "Display a quotation from the 'fortune' program."
  (interactive)
  (when (executable-find "fortune")
    (message (s-trim (shell-command-to-string "fortune")))))

(hook-fn 'after-init-hook
  (run-with-idle-timer 0.2 nil 'fortune))

;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:

;;; init.el ends here
