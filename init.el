;;; Disable intrusive GUI elements.
(scroll-bar-mode -1)
(menu-bar-mode   -1)
(tool-bar-mode   -1)

(defun cb:byte-compile-lisp ()
  "Recompile all configuration files."
  (interactive)
  (byte-recompile-directory cb:lisp-dir 0 t))

(defun cb:auto-mode-on-match (mode &rest regexes)
  "Use the provided major mode for files matching the given regex."
  (--each regexes
    (add-to-list 'auto-mode-alist `(,it . ,mode))))

;;; ----------------------------------------------------------------------------
;;; Initialize packages.

(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(defun cb:require-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;;; ----------------------------------------------------------------------------
;;; Load path

(add-to-list 'load-path (concat user-emacs-directory "lib"))
(add-to-list 'load-path user-emacs-directory)
(cb:require-package 'dash)
(require 'cb-load-path)

(cb:define-path cb:lib-dir  "lib/")
(cb:define-path cb:lisp-dir "lisp/")
(cb:define-path cb:tmp-dir  "tmp/")
(cb:define-path cb:etc-dir  "etc/")
(cb:define-path cb:bin-dir  "bin/")
(cb:define-path cb:yasnippet-dir "snippets/")
(cb:define-path cb:rsense-home "bin/rsense-0.3/")

;;; ----------------------------------------------------------------------------
;;; Packages

(require 'use-package)

(use-package s)

(use-package cl-lib)

(use-package cb-macros)

(use-package helm)

(use-package ido
  :config
  (progn
    (add-to-list 'ido-ignore-buffers "*helm mini*")
    (add-to-list 'ido-ignore-files "\\.DS_Store")

    (ido-mode +1)
    (ido-ubiquitous-mode +1)
    (icomplete-mode +1)

    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-max-prospects 10
          ido-default-file-method 'selected-window)))

(use-package smex
  :config (smex-initialize)
  :bind   (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)))

(use-package popwin
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
    (setq save-place-file (concat cb:tmp-dir "places"))
    (setq-default save-place t)))

(use-package paren
  :config (show-paren-mode +1))

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (concat cb:tmp-dir "recentf")
          recentf-max-saved-items 200
          recentf-exclude '(".newsrc"
                            "ede-projects.el"
                            ".ido.last"
                            ".emacs.d/session."
                            "Map_Sym.txt"))
    (recentf-mode +1)))


(use-package savehist
  :config
  (progn
    (setq savehist-additional-variables '(search ring regexp-search-ring)
          savehist-autosave-interval    60
          savehist-file                 (concat cb:tmp-dir "savehist"))
    (savehist-mode +1)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config   (global-undo-tree-mode +1))

(use-package window-number
  :config (window-number-meta-mode t))

(use-package winner
  :config (winner-mode +1))

(use-package volatile-highlights)

(use-package diminish
  :commands (diminish))

(use-package hl-line
  :config (global-hl-line-mode t))

(use-package fringe
  :if     (display-graphic-p)
  :config (fringe-mode '(2 . 0)))

(use-package transpose-frame
  :commands
  (transpose-frame
   flip-frame
   flop-frame
   rotate-frame
   rotate-frame-clockwise
   rotate-frame-anticlockwise))

(use-package ansi-color)

(use-package key-chord
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

(use-package cb-foundation)

(use-package evil
  :config
  (progn
    (use-package surround
      :config (global-surround-mode +1))
    (use-package evil-numbers
      :config (progn
                (define-key evil-normal-state-map (kbd "-") 'evil-numbers/inc-at-pt)
                (define-key evil-normal-state-map (kbd "+") 'evil-numbers/dec-at-pt)))
    (use-package cb-evil)
    (evil-mode +1)))

(use-package cb-osx :if (equal system-type 'darwin))

(use-package cb-colour)

(use-package ediff
  :commands (ediff ediff-merge-files-with-ancestor)
  :config
  (progn
    (setq diff-switches "-u"
          ediff-window-setup-function 'ediff-setup-windows-plain)
    (add-hook 'ediff-startup-hook 'turn-off-evil-mode)))

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
  (add-hook 'window-configuration-change-hook
            (lambda ()
              "Change process window size."
              (when (derived-mode-p 'comint-mode)
                (set-process-window-size (get-buffer-process (current-buffer))
                                         (window-height)
                                         (window-width))))
            nil t))

(use-package cb-ediff
  :commands (cb:handle-git-merge))

(use-package auto-complete
  :config
  (progn
    (require 'auto-complete-config)
    (ac-config-default)
    (global-auto-complete-mode t)
    (ac-flyspell-workaround)
    (ac-linum-workaround)
    (require 'cb-auto-complete)
    (add-hook 'text-mode-hook (lambda () (auto-complete-mode -1)))))

(use-package cb-google
  :commands (google/search)
  :bind     (("C-c C-/" . google/search)
             ("C-c C-_" . google/search)))

(use-package autopair
  :config
  (progn
    (setq autopair-autowrap t)
    (autopair-global-mode t)
    (defadvice autopair-mode (around cb:inhibit-on-paredit activate)
      "Prevent autopair from being used if paredit is active."
      (unless (and (boundp 'paredit-mode) paredit-mode)
        ad-do-it))))

;; FIXME
(require 'cb-indentation)

(use-package json-mode
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
  :mode (("\\.md$"          . mardown-mode)
         ("\\.[mM]arkdown$" . markdown-mode)))

(use-package mode-compile
  :bind (("C-c C-k" . mode-compile-kill)
         ("C-c C-c" . mode-compile))
  :config
  (progn
    (setq mode-compile-expert-p t
          mode-compile-always-save-buffer-p t
          compilation-window-height 12
          compilation-scroll-output 'first-error)

    (add-to-list 'compilation-finish-functions
                 (lambda (buf str)
                   "Close compilation buffer if compilation succeeded."
                   (unless (string-match "exited abnormally" str)
                     (delete-windows-on (get-buffer-create "*compilation")))))))

(defun cb:add-compilation-handler (mode handler &optional quitter)
  "Register a set of compilation handlers for the given mode."
  (add-to-list 'mode-compile-modes-alist `(,mode . (,handler ,quitter))))

(use-package flyspell-lazy
  :config
  (progn
    (flyspell-lazy-mode +1)
    (setq flyspell-issue-message-flag nil)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

;;; Set error navigation keybindings.
(defun cb:configure-error-nav ()
  (local-set-key (kbd "M-N") 'next-error)
  (local-set-key (kbd "M-P") 'previous-error))

(add-hook 'prog-mode-hook 'cb:configure-error-nav)
(add-hook 'text-mode-hook 'cb:configure-error-nav)

(use-package flycheck
  :config
  (progn
    (setq flycheck-highlighting-mode 'lines)
    (add-hook 'prog-mode-hook 'cb:maybe-enable-flycheck)
    (add-hook 'text-mode-hook 'cb:maybe-enable-flycheck)))

(defun cb:maybe-enable-flycheck ()
  (when (flycheck-may-enable-mode)
    (unless (equal major-mode 'emacs-lisp-mode)
      (flycheck-mode t))))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (setq yas/trigger-key (kbd "RET"))
    (add-to-list 'yas-snippet-dirs cb:yasnippet-dir)
    (yas--initialize)
    (yas-global-mode t)
    (add-hook 'yasnippet-mode-hook (lambda () (setq require-final-newline nil)))))

(use-package make-mode
  :config
  (progn
    (add-to-list 'ac-modes 'makefile-mode)
    (add-hook 'makefile-mode-hook
              (lambda ()
                (auto-complete-mode t)
                (setq indent-tabs-mode t)))))

(use-package cb-shebang)

(use-package nxml-mode
  :commands (nxml-mode)
  :config
  (add-hook 'find-file-hook
            (lambda ()
              "Enable nxml-mode if this is an XML file."
              (when (or (s-ends-with? ".xml" (buffer-file-name))
                        (s-starts-with? "<?xml " (buffer-string)))
                (nxml-mode)
                (local-set-key (kbd "M-q") 'cb:reformat-xml)))))

(use-package tagedit
  :commands (tagedit-add-paredit-like-keybindings)
  :config
  (add-hook 'html-mode-hook
            (lambda ()
              (tagedit-add-paredit-like-keybindings)
              (setq sgml-xml-mode +1))))

(use-package magit
  :bind ("C-c g" . magit-status)
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

    (add-hook 'magit-log-edit-mode-hook
              (lambda ()
                "Enter insertion mode."
                (when (and (boundp 'evil-mode) evil-mode)
                  (evil-append-line nil))))

    (add-hook 'magit-mode-hook 'magit-load-config-extensions)))

(use-package conf-mode
  :mode ((".gitignore$" . conf-mode)
         (".gitmodules$" . conf-mode)))

(use-package cb-magit)

(use-package paredit
  :commands (paredit-mode enable-paredit-mode disable-paredit-mode)
  :config   (use-package cb-paredit))

(use-package highlight)

(use-package lively)

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode)

(use-package highlight-symbol)

(use-package volatile-highlights
  :diminish volatile-highlights-mode)

(use-package eval-sexp-fu)

(use-package cb-lisp)

(use-package eldoc
  :commands (eldoc-mode)
  :diminish (eldoc-mode))

(use-package lisp-mode
  :commands (emacs-lisp-mode lisp-mode)
  :config
  (progn
    (use-package cb-elisp)
    (use-package ert)
    (add-hook 'emac-lisp-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c C-t") 'ert)))
    (add-hook 'after-save-hook
              (lambda ()
                "Byte compile elisp files on save."
                (when (and (equal major-mode 'emacs-lisp-mode)
                           (buffer-file-name))
                  (byte-compile-file (buffer-file-name)))))))

(use-package cb-lisp
  :commands ())

(use-package cb-clojure)

(use-package clojure-mode
  :commands (clojure-mode)
  :mode     ("\\.cljs?$" . clojure-mode)
  :config
  (progn
    (use-package midje-mode
      :commands (midje-mode)
      :diminish midje-mode
      :config   (add-hook 'clojure-mode-hook 'midje-mode))
    (add-hook 'clojure-mode-hook
              (lambda () (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl)))))

(use-package nrepl
  :commands (nrepl-jack-in)
  :config
  (progn
    (use-package ac-nrepl)
    (setq nrepl-popup-stacktraces    nil
          nrepl-hide-special-buffers t)

    (add-to-list 'ac-modes 'nrepl-mode)
    (set-face-attribute 'nrepl-error-highlight-face t :inherit 'error)
    (set-face-underline 'nrepl-error-highlight-face nil)

    (add-hook 'clojure-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl)
                (local-set-key (kbd "C-h f")   'nrepl-doc)
                (local-set-key (kbd "C-c C-f") 'nrepl-eval-buffer)))

    (--each '(nrepl-mode-hook nrepl-interaction-mode-hook)
      (add-hook it (lambda ()
                     (nrepl-turn-on-eldoc-mode)
                     (paredit-mode +1)
                     (local-set-key (kbd "C-c C-z") 'cb:switch-to-last-clj-buffer)
                     (local-set-key (kbd "C-c C-f") 'cb:eval-last-clj-buffer))))))

;; (require 'cb-overtone)

(use-package fsharp-mode
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
  :commands (python-mode)
  :config
  (progn
    (add-to-list 'ac-modes 'python-mode)
    (add-to-list 'ac-modes 'inferior-python-mode)))

(use-package ruby-mode
  :modes (("\\.rake$"    . ruby-mode)
          ("Rakefile$"   . ruby-mode)
          ("\\.gemspec$" . ruby-mode))
  :config
  (progn

    (use-package ruby-electric
      :diminish ruby-electric-mode
      :config
      (progn
        (setq ruby-electric-expand-delimiters-list '(39 96 124))
        (add-hook 'ruby-mode-hook 'ruby-electric-mode)))

    (use-package rsense
      :config
      (progn
        (setq rsense-home cb:rsense-home)
        (cb:define-path cb:rsense-home "bin/rsense-0.3")
        (add-to-list 'ac-sources 'ac-source-rsense-method)
        (add-to-list 'ac-sources 'ac-source-rsense-constant)))

    (use-package inf-ruby
      :config
      (progn
        (add-hook 'ruby-mode-hook 'ruby-electric-mode)
        (add-hook 'inf-ruby-mode 'inf-ruby-setup-keybindings)))

    (add-to-list 'ac-modes 'ruby-mode)
    (add-to-list 'completion-ignored-extensions ".rbc")))

(use-package haskell-mode
  :mode
  (("\\.hs"     . haskell-mode)
   ("\\.hsc$"   . haskell-c-mode)
   ("\\.cabal$" . haskell-cabal-mode))
  :config
  (progn

    (use-package ghc)

    (use-package haskell-edit)

    (use-package haskell-indentation
      :diminish haskell-indentation-mode)

    (use-package haskell-doc
      :diminish haskell-doc-mode)

    (use-package haskell-decl-scan)

    (use-package outline
      :commands (outline-mode)
      :diminish outline-mode
      :config
      (add-hook 'haskell-mode-hook 'outline-mode))

    (use-package hs-lint
      :config
      (setq hs-lint-command (executable-find "hlint")))

    (use-package scion
      :diminish scion-mode
      :config
      (progn
        (setq scion-log-events nil
              scion-program    (executable-find "scion-server"))))

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

    (add-hook 'haskell-mode-hook
              (lambda ()
                "Configure haskell auto-complete sources."
                (setq ac-sources (list 'ac-source-words-in-same-mode-buffers
                                       'ac-source-ghc-mod))))))

;; Local Variables:
;; no-byte-compile: t
;; End:
