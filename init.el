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
(menu-bar-mode (if (display-graphic-p) +1 -1))

;;; Fully-qualify `user-emacs-directory'.
(setq user-emacs-directory (expand-file-name user-emacs-directory))
(defvar user-home-directory (getenv "HOME"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; ============================================================================
;;; Initialize packages.

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

(defadvice use-package-ensure-elpa (around ignore-errs activate)
  "Ignore errors caused by package generation."
  (condition-case err
      ad-do-it
    (file-already-exists)))

(use-package diminish
  :ensure t
  :commands diminish)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package cb-macros)

(use-package cl-lib)

;;; ============================================================================

;;; Initialize paths

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

;; Use the version of emacs in /src for info and source.
(setq source-directory (format "%s/emacs-%s.%s" cb:src-dir
                               emacs-major-version
                               emacs-minor-version))
(setenv "INFOPATH" (concat source-directory "/info/"))

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
 initial-major-mode           'fundamental-mode
 initial-scratch-message      nil
 x-select-enable-clipboard    t
 font-lock-maximum-decoration t
 ring-bell-function           'ignore
 initial-scratch-message      nil
 truncate-partial-width-windows     nil
 confirm-nonexistent-file-or-buffer nil
 vc-handled-backends          '(Git)
 )
(setq-default
 tab-width                    4
 indent-tabs-mode             nil
 fill-column                  80)
(icomplete-mode +1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Typefaces

(defun cb:font (&rest fonts)
  "Return the first available font in FONTS."
  (--first (find-font (font-spec :name it)) fonts))

(defvar cb:serif-font
  (cb:font "Palatino" "Cambria" "Times New Roman"))

(defvar cb:sans-serif-font
  (cb:font "Lucida Grande" "Ubuntu Regular" "Segoe UI"
           "Helvetica Neue" "Calibri" "Helvetica" "Verdana" "Arial"))

(defvar cb:monospace-font
  (cb:font "Menlo" "Consolas" "Inconsolata" "DejaVu Sans Mono"
           "Ubuntu Mono Regular" "Courier"))

(hook-fn 'text-mode-hook
  "Use a sans-serif font for text-mode."
  (when (equal major-mode 'text-mode)
    (buffer-face-set `(:family ,cb:sans-serif-font :height 120))))

(hook-fn 'Info-mode-hook
  (buffer-face-set `(:family ,cb:serif-font :height 140)))

(set-frame-font (format "%s 11" cb:monospace-font)
                nil t)

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

;;; Exiting Emacs
;;;
;;; Rebind to C-c r q ("really quit"), so you're less likely to kill Emacs
;;; accidently with Org key-commands.
(bind-key* "C-x C-c" (lambda () (interactive) (message "Type <C-c r q> to exit Emacs")))
(bind-key* "C-c r q" 'save-buffers-kill-emacs)

;;; Help commands

(define-prefix-command 'help-find-map)
(bind-key "C-h e"   'help-find-map)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)
(bind-key "C-h e p" 'find-library)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e a" 'apropos)
(bind-key "C-h e V" 'apropos-value)

;;; Narrowing

(defun cb:narrow-dwim ()
  "Perform a context-sensitive narrowing command."
  (interactive)
  (cond ((buffer-narrowed-p)
         (widen))

        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        (t
         (narrow-to-defun))))

(bind-key "M-n" 'cb:narrow-dwim)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Misc commands
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "s-`"     'other-window)

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

;;; ============================================================================
;;; Modeline

(cl-defun cb:vc-state->letter (&optional (file (buffer-file-name)))
  "Return a single letter to represent the current version-control status."
  (case (ignore-errors (vc-state file))
    ((edited)               "M")
    ((needs-merge conflict) "!")
    ((missing unregistered) "?")
    ((added)                "A")
    ((removed)              "D")
    (t                      " ")))

(cl-defun cb:vc-file-uptodate? (&optional (file (buffer-file-name)))
  "Non-nil if FILE is up-to-date."
  (ignore-errors (equal 'up-to-date (vc-state file))))

(defun cb:shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    ;; Ellipsize the path if it is too long.
    ;; `2` is the length of the path separator + ellipsis.
    (while (and path (< (length output) (- max-length 2)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; Extra mode line faces

(defface mode-line-read-only-face
  '((((type graphic))
     (:foreground "#4271ae"
      :box '(:line-width 2 :color "#4271ae")))
    (t (:inherit 'mode-line-face)))
  "Face for readonly indicator."
  :group 'modeline)

(defface mode-line-modified-face
  '((((type graphic))
     (:foreground "#c82829"))
    (t
     (:inherit 'mode-line-face)))
  "Face for modified indicator."
  :group 'modeline)

(defface mode-line-directory-face
  '((((type graphic) (background dark))
     (:foreground "gray60"))
    (((type graphic) (background light))
     (:foreground "gray70"))
    (t
     (:inherit 'mode-line-face)))
  "Face for the directory component of the current filename."
  :group 'modeline)

(defface mode-line-filename-face
  '((((type graphic) (background dark))
     (:foreground "#eab700" :weight bold))
    (((type graphic) (background light))
     (:foreground "gray40" :weight bold))
    (t
     (:inherit 'mode-line-face)))
  "Face for the name component of the current filename."
  :group 'modeline)

(defface mode-line-position-face
  `((((type graphic) (background dark))
     (:family ,cb:monospace-font
      :height 100
      :foreground "gray60"))
    (((type graphic) (background light))
     (:family ,cb:monospace-font
      :height 100
      :foreground "gray50"))
    (t
     (:inherit 'mode-line-face)))
  "Face for the position indicators."
  :group 'modeline)

(defface mode-line-mode-face
  '((((type graphic) (background dark))
     (:foreground "gray70"))
    (((type graphic) (background light))
     (:foreground "gray40"))
    (t
     (:inherit 'mode-line-face)))
  "Face for the current major mode indicator."
  :group 'modeline)

(defface mode-line-minor-mode-face
  '((((type graphic) (background dark))
     (:foreground "gray40" :height 110))
    (((type graphic) (background light))
     (:foreground "gray70" :height 110))
    (t (:inherit 'mode-line-mode-face)))
  "Face for the current minor mode indicators."
  :group 'modeline)

(defface mode-line-process-face
  '((((type graphic))
     (:foreground "#718c00"))
    (t
     (:inherit 'mode-line-face)))
  "Face for the current process."
  :group 'modeline)

(defface mode-line-80col-face
  '((((type graphic) (background dark))
     (:foreground "#eab700"))
    (((type graphic) (background light))
     (:foreground "#b58900"))
    (t
     (:inherit 'mode-line-position-face)))
  "Face for the warning when point is past column 80."
  :group 'modeline)

(setq-default
 mode-line-format
 '(
   ;; Position, including warning for 80 columns
   (:propertize " %4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ;; emacsclient [default -- keep?]
   mode-line-client
   " "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((or (buffer-modified-p)
               (not (cb:vc-file-uptodate?)))
           (propertize (format " %s%s "
                               (cb:vc-state->letter)
                               (if (buffer-modified-p) "*" " "))
                       'face 'mode-line-modified-face))
          (t "    ")))
   " "
   ;; directory and buffer/file name
   (:propertize (:eval (if (buffer-file-name)
                           (cb:shorten-directory default-directory 30)
                         ""))
                face mode-line-directory-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n "

   ;; mode indicators: recursive edit, major mode, minor modes, process, global

   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "

   ;; Show ert test status.
   (:eval (when (and (boundp 'ert-modeline-mode) ert-modeline-mode)
            (set-face-bold 'ertml-failing-face t)
            (let ((s (s-trim ertml--status-text)))
              (if (s-matches? (rx digit) s)
                  (propertize s 'face 'ertml-failing-face)
                (propertize s 'face 'bold)))))

   ;; Show minor modes.
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)))

;;; ============================================================================
;;; Mode groups

(defmacro cb:define-mode-group (name modes)
  "Create an ad-hoc relationship between language modes.
* Creates a special var with NAME to contain the grouping.
* Declares a hook NAME-hook that runs after any of MODES are initialized."
  (declare (indent 1))
  (let ((hook (intern (format "%s-hook" name))))
    `(progn
       ;; Define modes variable.
       (defconst ,name ,modes "Auto-generated variable for language grouping.")
       ;; Define hook variable.
       (defvar ,hook nil "Auto-generated hook for language grouping.")
       ;; Add combined hook to each mode's hook.
       (--each ,modes
         (hook-fn (intern (concat (symbol-name it) "-hook"))
           (run-hooks ',hook)))
       ',name)))

(cb:define-mode-group cb:scheme-modes
  '(scheme-mode
    inferior-scheme-mode
    geiser-repl-mode
    geiser-mode))

(cb:define-mode-group cb:clojure-modes
  '(clojure-mode
    clojurescript-mode))

(cb:define-mode-group cb:elisp-modes
  '(emacs-lisp-mode
    ielm-mode))

(cb:define-mode-group cb:slime-modes
  '(slime-mode
    slime-repl-mode))

(cb:define-mode-group cb:lisp-modes
  `(,@cb:scheme-modes
    ,@cb:clojure-modes
    ,@cb:elisp-modes
    ,@cb:slime-modes
    common-lisp-mode
    lisp-mode
    repl-mode))

(cb:define-mode-group cb:haskell-modes
  '(haskell-mode
    inferior-haskell-mode
    haskell-interactive-mode
    haskell-c-mode
    haskell-cabal-mode))

(cb:define-mode-group cb:python-modes
  '(python-mode
    inferior-python-mode))

(cb:define-mode-group cb:ruby-modes
  '(inf-ruby-mode
    ruby-mode))

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

;;; Shebang insertion

(defconst cb:extension->cmd (make-hash-table :test 'equal))
(puthash "py" "python" cb:extension->cmd)
(puthash "sh" "bash"   cb:extension->cmd)
(puthash "rb" "ruby"   cb:extension->cmd)
(puthash "el" "emacs"  cb:extension->cmd)

(defun cb:bufname->cmd (name)
  (gethash (car-safe (last (split-string name "[.]" t)))
           cb:extension->cmd))

(defun insert-shebang ()
  "Insert a shebang line at the top of the current buffer."
  (interactive)
  (let* ((env (shell-command-to-string "where env"))
         (env (replace-regexp-in-string "[\r\n]*" "" env))
         (cmd (cb:bufname->cmd buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (insert (concat "#!" env " " cmd))
      (newline 2))))

;;; Forward-declare ac-modes so auto-complete can be safely loaded separately
;;; from other modes.
(defvar ac-modes nil)

;;; ----------------------------------------------------------------------------

;;; OS X-specific configuration.
(when (equal system-type 'darwin)

  ;; Set exec-path
  (use-package exec-path-from-shell
    :ensure t
    :if     (display-graphic-p)
    :config (exec-path-from-shell-initialize))

  ;; Configure cut & paste in terminal.
  (unless (window-system)

    (defun cb:paste ()
      (shell-command-to-string "pbpaste"))

    (defun cb:copy (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq interprogram-cut-function   'cb:copy
          interprogram-paste-function 'cb:paste)))

;;; ----------------------------------------------------------------------------

(use-package simple
  :diminish (visual-line-mode
             global-visual-line-mode
             auto-fill-mode)
  :config
  (add-hook 'text-mode-hook 'visual-line-mode))

;;; Helm

(use-package projectile
  :ensure   t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode)
    (defadvice find-tag (before set-tags-directory activate)
      "Ensure the TAGS path is set before searching for tags."
      (setq tags-file-name (concat (projectile-project-root) "TAGS")))))

(use-package helm
  :ensure   t
  :init
  (progn
    (bind-key* "M-a" 'helm-apropos)
    (bind-key* "M-b" 'helm-buffers-list)
    (bind-key* "C-x C-b" 'helm-buffers-list)
    (bind-key* "M-j" 'helm-mini)
    (bind-key* "M-i" 'helm-imenu)
    (bind-key* "M-f" 'helm-etags-select)
    (bind-key* "M-m" 'helm-man-woman)
    (bind-key* "M-w" 'helm-w3m-bookmarks)
    (bind-key* "M-k" 'helm-show-kill-ring))
  :config
  (define-key helm-map (kbd "C-[") 'helm-keyboard-quit))

(use-package helm-projectile
  :ensure t
  :commands helm-projectile
  :init
  (progn
    (defun cb:helm-dwim ()
      "Show helm-projectile, failling back to helm-mini if not in a project."
      (interactive)
      (if (projectile-project-p)
          (helm-projectile)
        (helm-mini)))

    (bind-key* "C-j" 'cb:helm-dwim)))

;;; Ido

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

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind   (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)))

;;; Buffer/window management

(use-package workgroups
  :if       (display-graphic-p)
  :bind     (("s-1" . wg-switch-to-index-0)
             ("s-2" . wg-switch-to-index-1)
             ("s-3" . wg-switch-to-index-2)
             ("s-4" . wg-switch-to-index-3)
             ("s-5" . wg-switch-to-index-4)
             )
  :ensure   t
  :diminish workgroups-mode
  :commands workgroups-mode
  :init
  (progn
    (setq
     wg-morph-vsteps 6
     wg-prefix-key (kbd "C-c w")
     wg-default-session-file (concat cb:etc-dir "workgroups"))
    (add-hook 'after-init-hook 'workgroups-mode))
  :config
  (progn
   (defadvice wg-mode-line-add-display (around wg-suppress-error activate)
     "Ignore errors in modeline display function caused by custom modeline."
     (ignore-errors ad-do-it))
    (ignore-errors (wg-load wg-default-session-file))))

(use-package popwin
  :ensure t
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

(use-package winner
  :config (winner-mode +1))

(use-package window-number
  :ensure t
  :config (window-number-meta-mode +1))

(use-package windmove
  :bind (("S-<left>"  . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>"    . windmove-up)
         ("S-<down>"  . windmove-down)))

(use-package cb-commands
  :bind (("s-f"     . cb:rotate-buffers)
         ("C-x C-o" . other-window))

  :commands (cb:hide-dos-eol
             cb:kill-current-buffer
             indent-buffer
             insert-timestamp
             rename-file-and-buffer
             cb:last-buffer-for-mode)

  :init     (add-hook 'find-file-hook 'cb:hide-dos-eol)

  :config
  (defadvice cb:rotate-buffers (after select-largest-window activate)
    "Switch to the largest window if using a 2-up window configuration."
    (when (= 2 (length (window-list)))
      (cb:select-largest-window))))

;;; Backups & State

(use-package saveplace
  :init
  (progn
    (setq save-place-file (concat cb:tmp-dir "saved-places"))
    (setq-default save-place t)))

(use-package recentf
  :init
  (setq
   recentf-save-file       (concat cb:tmp-dir "recentf")
   recentf-keep            '(file-remote-p file-readable-p)
   recentf-max-saved-items 100
   recentf-max-menu-items  25
   recentf-exclude '(".newsrc"
                     "Emacs.app"
                     "-autoloads.el"
                     "recentf"
                     ".ido.last"
                     "TAGS"
                     ".gz")))

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

(use-package savehist
  :config
  (progn
    (setq
     savehist-additional-variables '(search ring regexp-search-ring)
     savehist-autosave-interval    60
     savehist-file                 (concat cb:tmp-dir "savehist"))
    (savehist-mode +1)))

(use-package undo-tree
  :ensure   t
  :diminish undo-tree-mode
  :config   (global-undo-tree-mode +1))

;;; Cosmetic

(use-package highlight
  :ensure t
  :defer t)

(use-package hl-line
  :if (display-graphic-p)
  :config (global-hl-line-mode t))

(use-package fringe
  :if     (display-graphic-p)
  :config (fringe-mode '(2 . 0)))

(use-package ansi-color
  :config
  (progn

    (defadvice display-message-or-buffer (before ansi-color activate)
      "Process ANSI color codes in shell output."
      (let ((buf (ad-get-arg 0)))
        (and (bufferp buf)
             (string= (buffer-name buf) "*Shell Command Output*")
             (with-current-buffer buf
               (ansi-color-apply-on-region (point-min) (point-max))))))

    (ansi-color-for-comint-mode-on)))

;;; Navigation

(use-package ace-jump-mode
  :ensure t
  :bind (("C-L" . ace-jump-line-mode)
         ("C-SPC" . ace-jump-word-mode)
         ;; Needed for terminal.
         ("C-@" . ace-jump-word-mode))
  :config
  (progn
    (hook-fn 'ace-jump-mode-end-hook
      (ignore-errors
        (exit-recursive-edit)))

    ;; Use ESC to quit ace-jump.
    (--each '(ace-jump-line-mode ace-jump-word-mode ace-jump-char-mode)
      (hook-fn it (local-set-key (kbd "ESC") 'keyboard-quit)))))

(use-package hideshow
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :defer    t)

;;; Web

(use-package smtpmail
  :commands smtpmail-send-it
  :init
  (setq
   mail-signature (concat "\nCheers,\n\n" user-full-name)
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it

   smtpmail-mail-address user-mail-address
   smtpmail-smtp-server "smtp.mail.me.com"
   smtpmail-smtp-service 587))

(use-package gnus
  :commands gnus
  :defer t
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

(use-package bbdb
  :ensure t
  :defer  t
  :init
  (progn
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
    (add-hook 'message-mode-hook 'bbdb-insinuate-mail)
    (setq bbdb-file "~/Dropbox/.bbdb"))
  :config
  (progn
    (setq
     bbdb-offer-save 1
     bbdb-use-popup  t
     bbdb-electric   t
     bddb-popup-target-lines     1
     bbdb-dwim-net-address-allow-redundancy t
     bbdb-quiet-about-name-mismatches       2
     bbdb-always-add-address     t
     bbdb-canonicalize-redundant-nets-p     t
     bbdb-completion-type nil
     bbdb-complete-name-allow-cycling       t
     bbbd-message-caching-enabled           t
     bbdb-use-alternate-names    t
     bbdb-elided-display         t
     bbdb/mail-auto-create-p     'bbdb-ignore-some-messages-hook
     )
    (bbdb-initialize)))

(use-package bbdb-vcard
  :commands (bbdb-vcard-import-file
             bbdb-vcard-import-buffer
             bbdb-vcard-export)
  :config
  ;; HACK: calls functions that appear not to exist.
  (progn
    (defalias 'bbdb-record-Notes 'ignore)
    (defalias 'bbdb-record-set-Notes 'ignore)))

(use-package google-this
  :ensure   t
  :commands google-this
  :diminish google-this-mode
  :init     (bind-key* "M-s" 'google-this)
  :config   (google-this-mode +1))

(use-package w3m
  :ensure   t
  :bind ("M-e" . w3m-browse-url)
  :commands (w3m-find-file w3m-browse-url)
  :init
  (progn
    (setq browse-url-browser-function 'w3m-browse-url)

    (defun cb:find-window-with-mode (mode)
      "Find the first window whose buffer is in major-mode MODE."
      (get-window-with-predicate
       (lambda (w) (with-current-buffer (window-buffer w)
                     (equal mode major-mode)))))

    (defun cb:w3m-browse-url-as-help (url)
      "Browse the given URL in a help window."
      (interactive "sURL: ")
      (let ((win (or (cb:find-window-with-mode 'w3m-mode) (split-window))))
        (select-window win)
        (w3m-browse-url url)))
    )
  :config
  (hook-fn 'w3m-mode-hook
    (buffer-face-set
     `(:family ,cb:serif-font :height 130))))

(use-package erc
  :defer t
  :config
  (progn
    (erc-autojoin-mode +1)
    (erc-track-mode +1)
    (setq
     erc-hide-list '("JOIN" "PART" "QUIT" "NICK")

     erc-track-exclude-types
     '("JOIN" "NICK" "PART" "QUIT" "MODE"
       "324" "329" "332" "333" "353" "477")

     erc-autojoin-channels-alist
     '((".*\\.freenode.net" "#emacs" "#haskell")))))

;;; Themes

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

;;; Vim & Evil

(use-package evil
  :ensure t
  :commands evil-mode

  :init
  (progn
    (defun cb:append-buffer ()
      "Enter insertion mode at the end of the current buffer."
      (interactive)
      (goto-char (point-max))
      (when (fboundp 'evil-append-line)
        (evil-append-line 1)))

    (evil-mode +1)
    (evil-global-set-key 'insert (kbd "S-TAB") 'tab-to-tab-stop)
    (add-hook 'comint-mode-hook 'cb:append-buffer))

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

    (progn-after-load "man"
      (evil-declare-key 'normal Man-mode-map (kbd "q") 'Man-kill))

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
  :ensure   t
  :commands evil-paredit-mode
  :init     (add-hook 'paredit-mode-hook 'evil-paredit-mode))

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

;;; Shells

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
  :bind ("M-t" . cb:shell-cycle)
  :init
  (defun cb:shell-cycle ()
    "Toggle various shell window states."
    (interactive)
    (cond
     ;; If shell is maximized, hide it.
     ((and (derived-mode-p 'shell-mode)
           (equal 1 (length (window-list))))
      (bury-buffer))
     ;; If we're looking at the shell, maximize it.
     ((derived-mode-p 'shell-mode)
      (delete-other-windows))
     ;; Otherwise show the shell.
     (t (shell))))
  :config
  (progn

    (hook-fn 'shell-mode-hook
      (local-set-key (kbd "M->") 'cb:append-buffer)
      (setq ac-sources '(ac-source-filename)))

    (hook-fn 'window-configuration-change-hook
      "Change process window size."
      (when (derived-mode-p 'comint-mode)
        (set-process-window-size (get-buffer-process (current-buffer))
                                 (window-height)
                                 (window-width))))

    (defadvice shell (after move-to-end-of-buffer activate)
      "Move to the end of the shell buffer and enter insertion state."
      (cb:append-buffer))))

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

    (add-to-list 'ac-dictionary-directories
                 (concat user-emacs-directory "ac-dict"))

    (--each cb:lisp-modes (add-to-list 'ac-modes it))
    (setq
     ac-auto-show-menu t
     ac-dwim t
     ac-use-menu-map t
     ac-quick-help-delay 0.4
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

;;; Completion

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

(use-package fuzzy
  :ensure t)

;;; Dired

(use-package dired
  :defer   t
  :init
  (hook-fn 'dired-mode-hook
    (set (make-local-variable 'auto-revert-interval) 0.1)
    (auto-revert-mode +1))
  :config
  (progn
  (setq dired-auto-revert-buffer t)
    (when (equal system-type 'darwin)
      ;; Use GNU version of ls if available.
      (-when-let (gls (executable-find "gls"))
        (setq ls-lisp-use-insert-directory-program t
              insert-directory-program gls)))))

(use-package dired-aux
  :defer t
  :init
  (hook-fn 'dired-mode-hook
    (evil-local-set-key 'normal (kbd "TAB") 'dired-hide-subdir)
    (evil-local-set-key 'normal [backtab] 'dired-hide-all)
    (evil-local-set-key 'normal [backspace] 'dired-kill-subdir))
  :config
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(use-package dired-x
  :commands (dired-jump dired-jump-other-window)
  :init
  (progn
    (bind-key* "M-d" 'dired-jump)
    (bind-key* "M-D" 'dired-jump-other-window)))

(use-package dired-details
  :ensure   t
  :commands dired-details-install
  :init     (add-hook 'dired-mode-hook 'dired-details-install))

;;; Compilation & Checking

(use-package compile
  :bind  (("C-c b" . compile)
          ("C-c C-b" . recompile))
  :config
  (progn
    (defun cb:compile-autoclose (buffer string)
      "Automatically close the compile window."
      (when (-contains? '(compile recompile) last-command)
        (if (s-matches? "finished" string)
            (run-with-timer (/ 1 50) nil
                            '(lambda (w) (delete-window w) (message "Compilation succeeded"))
                            (get-buffer-window buffer t))
          (message "Compilation exited abnormally: %s" string))))

    (hook-fn 'find-file-hook
      "Try to find a makefile for the current project."
      (when (projectile-project-p)
        (setq-local compilation-directory (projectile-project-root))))

    (setq
     compilation-window-height    12
     compilation-scroll-output    'first-error
     compilation-finish-functions 'cb:compile-autoclose)))

(use-package mode-compile
  :ensure t
  :bind (("C-c ."   . mode-compile-kill)
         ("C-c C-c" . mode-compile))
  :config
  (setq mode-compile-expert-p             t
        mode-compile-always-save-buffer-p t))

(use-package flyspell
  :diminish flyspell-mode
  :defer    t
  :init
  (progn
    (setq ispell-dictionary "english")
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (bind-key* "C-'" 'flyspell-auto-correct-word))

(use-package flyspell-lazy
  :ensure  t
  :defines flyspell-lazy-mode
  :config  (flyspell-lazy-mode +1))

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

;;; Tags

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

;;; Language utils

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands (smartparens-mode smartparens-global-mode)
  :init
  (progn
    (add-hook 'text-mode-hook 'smartparens-mode)
    (add-hook 'comint-mode-hook 'smartparens-mode)
    (hook-fn 'prog-mode-hook
      "Ensure Paredit is used for Lisps."
      (if (-contains? cb:lisp-modes major-mode)
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
    (defun cb:smart-equals-dwim ()
      "Insert an '=' char padded by spaces, except in function arglists."
      (interactive)
      (if (string-match-p
           (rx (* space) "def ")
           (buffer-substring (line-beginning-position) (line-end-position)))
          (insert-string "=")
        (smart-insert-operator "=")))

    (hook-fn 'cb:python-modes-hook
      (smart-insert-operator-hook)
      (local-set-key (kbd "=") 'cb:smart-equals-dwim)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'cb:ruby-modes-hook
      (smart-insert-operator-hook)
      (local-set-key (kbd "=") 'cb:smart-equals-dwim)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'cb:haskell-modes-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd ":"))
      (local-unset-key (kbd ".")))

    (hook-fn 'sclang-mode-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd "|"))
      (local-unset-key (kbd ".")))

    (hook-fn 'asm-mode-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd "%"))
      (local-unset-key (kbd "-"))
      (local-unset-key (kbd ".")))

    (hook-fn 'c-mode-common-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":"))
      (local-unset-key (kbd "-"))
      (local-unset-key (kbd "+"))
      (local-set-key (kbd "*") 'c-electric-star))))

(use-package lambda-mode
  :diminish lambda-mode
  :config
  (progn
    (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
    (add-hook 'cb:scheme-modes-hook    'lambda-mode)
    (add-hook 'inferior-lisp-mode-hook 'lambda-mode)
    (add-hook 'lisp-mode-hook          'lambda-mode)
    (add-hook 'cb:elisp-modes-hook     'lambda-mode)
    (add-hook 'cb:python-modes-hook    'lambda-mode)
    (add-hook 'cb:slime-modes-hook     'lambda-mode)))

(use-package paren
  :config (show-paren-mode +1))

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

;;; XML

(use-package nxml-mode
  :commands nxml-mode
  :config
  (progn

    (defun cb:reformat-xml-in-region (begin end)
      (save-excursion
        (let ((end end))
          (nxml-mode)
          (goto-char begin)
          (while (search-forward-regexp "\>[ \\t]*\<" nil t)
            (backward-char)
            (insert "\n")
            (cl-incf end))
          (indent-region begin end))))

    (defun cb:reformat-xml ()
      "Insert newlines and indent XML. Operates on region, or the whole buffer if no region is defined."
      (interactive)
      (save-excursion
        (let ((start (or (ignore-errors (region-beginning))
                         (point-min)))
              (end   (or (ignore-errors (region-end))
                         (point-max))))
          (cb:reformat-xml-in-region start end))))

    (hook-fn 'find-file-hook
      "Enable nxml-mode if this is an XML file."
      (when (or (s-ends-with? ".xml" (buffer-file-name))
                (s-starts-with? "<?xml " (buffer-string)))
        (nxml-mode)
        (local-set-key (kbd "M-q") 'cb:reformat-xml)))))

(use-package tagedit
  :ensure   t
  :commands (tagedit-add-paredit-like-keybindings)
  :config
  (hook-fn 'html-mode-hook
    (tagedit-add-paredit-like-keybindings)
    (setq sgml-xml-mode +1)))

;;; Lisps

(use-package parenface-plus
  :ensure t)

(use-package eval-sexp-fu
  :commands eval-sexp-fu-flash-mode
  :init     (add-hook 'cb:lisp-modes-hook 'eval-sexp-fu-flash-mode)
  :config   (setq eval-sexp-fu-flash-duration 0.2))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (add-hook 'cb:lisp-modes-hook 'turn-on-eldoc-mode))

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
    (add-hook 'cb:lisp-modes-hook 'enable-paredit-mode)
    (add-hook 'inferior-lisp-mode-hook 'paredit-mode)
    (add-hook 'repl-mode-hook 'paredit-mode)))

(use-package slime
  :defer t
  :commands (slime-mode slime)
  :init
  (progn
    (setq slime-lisp-implementations `((lisp ("sbcl" "--noinform"))))

    (defun run-slime ()
      "Run slime, prompting for a lisp implementation."
      (interactive)
      (let ((current-prefix-arg '-))
        (slime))))

  :config
  (progn
    (require 'slime)
    (slime-setup '(slime-fancy))))

(use-package ac-slime
  :ensure   t
  :defer    t
  :commands (set-up-slime-ac)
  :init     (add-hook 'slime-modes-hook 'set-up-slime-ac)
  :config   (add-to-list 'ac-modes 'slime-repl-mode))

;;; Elisp

(use-package lisp-mode
  :defer t
  :config
  (progn

    (defun cb:switch-to-ielm ()
      "Start up or switch to an Inferior Emacs Lisp buffer."
      (interactive)
      ;; HACK: rebind switch-to-buffer so ielm opens in another window.
      (flet ((switch-to-buffer (buf) (switch-to-buffer-other-window buf)))
        (ielm)
        (cb:append-buffer)))

    (defun cb:last-elisp-buffer ()
      "Find the last active Elisp buffer."
      (--first (with-current-buffer it
                 (equal 'emacs-lisp-mode major-mode))
               (buffer-list)))

    (defun cb:switch-to-elisp ()
      "Switch to the last active elisp buffer."
      (interactive)
      (-when-let (buf (cb:last-elisp-buffer))
        (switch-to-buffer-other-window buf)))

    (add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))
    (define-key emacs-lisp-mode-map (kbd "C-c C-t") 'ert)
    (define-key emacs-lisp-mode-map (kbd "C-c e b") 'eval-buffer)
    (define-key emacs-lisp-mode-map (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'cb:switch-to-ielm)
    (define-key emacs-lisp-mode-map (kbd "C-c e r") 'eval-region)

    (hook-fn 'ielm-mode-hook (local-set-key (kbd "C-c C-z") 'cb:switch-to-elisp))

    (defun cb:elisp-after-save ()
      "Check parens are balanced and byte-compile."
      (check-parens)
      (ignore-errors
        (unless (equal "Carton" (file-name-nondirectory (buffer-file-name)))
          (byte-compile-file (buffer-file-name)))))

    (hook-fn 'emacs-lisp-mode-hook
      (add-hook 'after-save-hook 'cb:elisp-after-save t 'local))

    (defadvice eval-buffer (after buffer-evaluated-feedback activate)
      "Message that the buffer has been evaluated."
      (when (buffer-file-name)
        (message "Buffer evaluated.")))

    ;; Elisp font-locking
    (font-lock-add-keywords
     'emacs-lisp-mode
     `(
       ;; General keywords
       (,(rx "(" (group (or "use-package"
                            "hook-fn"
                            "progn-after-load"
                            "ac-define-source"
                            "flycheck-declare-checker"
                            "cl-destructuring-bind"
                            "cl-defstruct"))
             word-end)
        (1 font-lock-keyword-face))

       ;; Identifiers after keywords
       (,(rx "(" (group (or "use-package"
                            "ac-define-source"
                            "flycheck-declare-checker"))
             (+ space)
             (group (+ (regex "\[^ )\n\]"))))
        (2 font-lock-constant-face))

       ;; definition forms
       (,(rx "("
             (group (* (not space)) (or "cl-" "--" "/" ":") "def"
                    (* (not space)))
             (+ space)
             (group (+ (regex "\[^ )\n\]"))))
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face))

       ;; cb:extracting-list
       (,(rx "(" (group "cb:extracting-list") (or space eol))
        (1 font-lock-keyword-face))

       ;; cl-struct.
       (,(rx "(cl-defstruct"
             (+ space)
             (group (+ (regex "\[^ )\n\]"))))

        (1 font-lock-type-face))))))

(use-package ert-modeline
  :defer    t
  ;; The status is added to the mode-line format directly.
  :diminish ert-modeline-mode
  :commands ert-modeline-mode
  :init     (add-hook 'emacs-lisp-mode-hook 'ert-modeline-mode))

(use-package redshank
  :ensure   t
  :commands turn-on-redshank-mode
  :diminish redshank-mode
  :init     (add-hook 'cb:lisp-modes-hook 'turn-on-redshank-mode))

(use-package macrostep
  :ensure t
  :bind   ("C-c e m" . macrostep-expand)
  :config (evil-add-hjkl-bindings macrostep-mode-map 'motion))

(use-package elisp-slime-nav
  :ensure   t
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-mode
  :defer    t
  :init
  (hook-fn 'cb:elisp-modes-hook
    (elisp-slime-nav-mode +1)
    (local-set-key (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

    ;; Make M-. work in normal state.
    (when (featurep 'evil)
      (define-key evil-normal-state-map (kbd "M-.")
        'elisp-slime-nav-find-elisp-thing-at-point))))

(use-package litable
  :ensure   t
  :commands litable-mode
  :defer    t
  :init     (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'litable-mode))

(use-package emr
  :ensure t
  :bind   ("M-RET" . emr-show-refactor-menu)
  :config (require 'emr-elisp))

;;; Clojure

(use-package clojure-mode
  :ensure t
  :commands (clojure-mode)
  :mode     ("\\.cljs?$" . clojure-mode)
  :config
  (progn

    (defun cb:switch-to-nrepl ()
      "Start nrepl or switch to an existing nrepl buffer."
      (interactive)
      (-if-let (buf (get-buffer "*nrepl*"))
        (nrepl-switch-to-repl-buffer buf)
        (nrepl-jack-in)))

    (hook-fn 'clojure-mode-hook
      (subword-mode +1)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl))))

(use-package nrepl
  :ensure   t
  :commands nrepl-jack-in
  :config
  (progn

    (defadvice nrepl-switch-to-repl-buffer (after insert-at-end-of-nrepl-line activate)
      "Enter insertion mode at the end of the line when switching to nrepl."
      (cb:append-buffer))

    (defadvice back-to-indentation (around move-to-nrepl-bol activate)
      "Move to position after prompt in nREPL."
      (if (equal major-mode 'nrepl-mode)
          (nrepl-bol)
        ad-do-it))

    (defun cb:switch-to-clojure ()
      "Switch to the last active clojure buffer."
      (interactive)
      (-when-let (buf (cb:last-buffer-for-mode 'clojure-mode))
        (pop-to-buffer buf)))

    (defun cb:eval-last-clj-buffer ()
      "Evaluate that last active clojure buffer without leaving the repl."
      (interactive)
      (-when-let (buf (cb:last-buffer-for-mode 'clojure-mode))
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

(use-package cb-overtone
  :commands (maybe-enable-overtone-mode cb:stop-overtone)
  :init     (add-hook 'clojure-mode-hook 'maybe-enable-overtone-mode))

(use-package midje-mode
  :ensure   t
  :commands midje-mode
  :diminish midje-mode
  :init    (add-hook 'clojure-mode-hook 'midje-mode))

;;; Scheme

(use-package geiser
  :ensure t
  :commands run-geiser
  :init
  (progn-after-load "auto-complete"
    (autoload 'geiser-company--prefix-at-point "geiser-company")
    (autoload 'geiser-company--doc "geiser-company")

    (defun cb:geiser-eval-buffer ()
      "Evaluate the current Scheme buffer with Geiser."
      (interactive)
      ;; Switch to source if we're in the repl.
      (if (derived-mode-p 'repl-mode 'comint-mode 'inferior-scheme-mode)
          (save-excursion
            (switch-to-geiser)
            (cb:geiser-eval-buffer)
            (switch-to-geiser))

        (let (result)
          (flet ((message (&rest args) (setq result (apply 'format args))))
            (save-excursion
              (mark-whole-buffer)
              (geiser-eval-region (region-beginning) (region-end))))
          (message "Buffer evaluated %s" result))))

    (defun cb:geiser-ac-doc (fname &optional module impl)
      (let* ((symbol (intern fname))
             (impl (or impl geiser-impl--implementation))
             (module (geiser-doc--module (or module (geiser-eval--get-module))
                                         impl)))
        (-when-let (ds (geiser-doc--get-docstring symbol module))
          (ignore-errors
            (with-temp-buffer
              (geiser-doc--insert-title
               (geiser-autodoc--str* (cdr (assoc "signature" ds))))
              (newline)
              (insert (or (cdr (assoc "docstring" ds)) ""))
              (buffer-string))))))

    (ac-define-source geiser
      '((candidates . (progn
                        (geiser-company--prefix-at-point)
                        (cdr geiser-company--completions)))
        (document   . cb:geiser-ac-doc)))

    (hook-fn 'cb:scheme-shared-hook
      (local-set-key (kbd "C-c C-l") 'cb:geiser-eval-buffer)
      (setq ac-sources '(ac-source-yasnippet ac-source-geiser)))
    )
  :config
  (progn
    (defadvice switch-to-geiser (after append-with-evil activate)
      "Move to end of REPL and append-line."
      (when (derived-mode-p 'comint-mode)
        (cb:append-buffer)))

    (setq
     geiser-mode-start-repl-p t
     geiser-repl-startup-time 20000
     geiser-repl-history-filename (concat cb:tmp-dir "geiser-history")
     geiser-active-implementations '(racket))))

(use-package r5rs
  :ensure t
  :commands scheme-r5rs-lookup
  :init
  (progn
    (setq scheme-r5rs-root (concat cb:etc-dir "r5rs-html/"))
    (hook-fn 'cb:scheme-shared-hook
      (local-set-key (kbd "C-c C-h") 'scheme-r5rs-lookup)
      (set (make-local-variable 'browse-url-browser-function)
           (lambda (url &rest _)
             (cb:w3m-browse-url-as-help (concat "file://" url)))))))

;;; Python

(use-package python
  :ensure   t
  :commands python-mode
  :mode     ("\\.py$" . python-mode)
  :config
  (progn
    (defun cb:comma-then-space ()
      (interactive)
      (atomic-change-group
        (insert-char ?\,)
        (just-one-space)))

    (defun cb:switch-to-python ()
      "Switch to the last active Python buffer."
      (interactive)
      (-when-let (buf (cb:last-buffer-for-mode 'python-mode))
        (pop-to-buffer buf)))

    (define-key python-mode-map (kbd ",") 'cb:comma-then-space)
    (define-key inferior-python-mode-map (kbd ",") 'cb:comma-then-space)
    (define-key inferior-python-mode-map (kbd "C-c C-z") 'cb:switch-to-python)
    (add-to-list 'ac-modes 'python-mode)
    (add-to-list 'ac-modes 'inferior-python-mode)))

(use-package jedi
  :ensure   t
  :commands jedi:setup
  :init     (add-hook 'cb:python-modes-hook 'jedi:setup)
  :config   (setq jedi:setup-keys t))

;;; Ruby

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
    (add-to-list 'ac-modes 'ruby-mode)
    (add-to-list 'completion-ignored-extensions ".rbc")

    (defun cb:switch-to-ruby ()
      "Toggle between irb and the last ruby buffer.
Start an inferior ruby if necessary."
      (interactive)
      (if (derived-mode-p 'inf-ruby-mode)
          (switch-to-buffer-other-window
           (cb:last-buffer-for-mode 'ruby-mode))
        (run-ruby)
        (cb:append-buffer)))

    (hook-fn 'cb:ruby-modes-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-ruby)
      (subword-mode +1))))

(use-package rsense
  :ensure t
  :init
  (hook-fn 'cb:ruby-modes-hook
    (add-to-list 'ac-sources 'ac-source-rsense-method)
    (add-to-list 'ac-sources 'ac-source-rsense-constant))
  :config
  (progn
    (cb:define-path cb:rsense-home "bin/rsense-0.3")
    (setq rsense-home cb:rsense-home)))

(use-package yari
  :ensure t
  :commands yari
  :init
  (hook-fn 'cb:ruby-modes-hook
    (local-set-key (kbd "C-c C-h") 'yari)))

(use-package inf-ruby
  :ensure   t
  :commands inf-ruby-mode
  :init
  (hook-fn 'inf-ruby-mode
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

(use-package yaml-mode
  :ensure   t
  :commands yaml-mode
  :mode     (("\\.yaml$" . yaml-mode)
             ("\\.yml$"  . yaml-mode)))

;;; Haskell

(use-package haskell-mode
  :ensure t
  :commands (haskell-mode haskell-c-mode haskell-cabal-mode hoogle)
  :mode
  (("\\.hs$"    . haskell-mode)
   ("\\.hsc$"   . haskell-c-mode)
   ("\\.cabal$" . haskell-cabal-mode))
  :config
  (progn

    (defadvice switch-to-haskell (after insert-at-end-of-line activate)
      "Enter insertion mode at the end of the line when switching to inferior haskell."
      (cb:append-buffer))

    (add-to-list 'completion-ignored-extensions ".hi")

    (hook-fn 'cb:haskell-modes-hook
      (subword-mode +1)
      (local-set-key (kbd "C-c h") 'hoogle))

    (defun cb:switch-to-haskell ()
      "Switch to the last active Haskell buffer."
      (interactive)
      (-when-let (buf (cb:last-buffer-for-mode 'haskell-mode))
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
  (hook-fn 'cb:haskell-modes-hook
    (add-to-list 'ac-modes major-mode)
    (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
    (add-to-list 'ac-sources 'ac-source-haskell)))

(use-package haskell-edit
  :commands (haskell-find-type-signature
             haskell-reformat-type-signature))

(use-package haskell-indentation
  :diminish haskell-indentation-mode
  :init     (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package haskell-doc
  :diminish haskell-doc-mode
  :commands haskell-doc-mode
  :init     (add-hook 'cb:haskell-modes-hook 'haskell-doc-mode))

(use-package haskell-decl-scan
  :commands turn-on-haskell-decl-scan
  :init     (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan))

(use-package hs-lint
  :commands hs-lint
  :init
  (hook-fn 'haskell-mode-hook
    (local-set-key (kbd "C-c l") 'hs-lint))
  :config
  (setq hs-lint-command (executable-find "hlint")))

;;; C Languages

(use-package google-c-style
  :ensure t
  :defer t
  :commands google-set-c-style
  :init (add-hook 'c-mode-common-hook 'google-set-c-style))

(use-package disaster
  :ensure   t
  :commands disaster
  :defer    t
  :init
  (hook-fn 'c-mode-common-hook
    (local-set-key (kbd "C-c C-d") 'disaster)))

(use-package auto-complete-clang-async
  :commands ac-clang-launch-completion-process
  :init
  (hook-fn 'c-mode-common-hook
    (setq ac-sources '(ac-source-clang-async
                       ac-source-yasnippet
                       ac-source-words-in-buffer))
    (ac-clang-launch-completion-process))
  :config
  (progn
    (cb:define-path cb:clang-complete-dir "lib/clang-complete-async/")
    (setq ac-clang-complete-executable (concat cb:clang-complete-dir "clang-complete"))))

(use-package c-eldoc
  :ensure   t
  :commands c-turn-on-eldoc-mode
  :init     (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;;; SuperCollider

(use-package sclang
  :commands (sclang-mode sclang-start)
  :mode ("\\.sc$" . sclang-mode)
  :init
  (defun supercollider ()
    "Start SuperCollider and open the SC Workspace."
    (interactive)
    (switch-to-buffer
     (get-buffer-create "*sclang workspace*"))
    (sclang-mode))

  :config
  (progn
    (setq sclang-auto-scroll-post-buffer   t
          sclang-eval-line-forward         nil
          sclang-show-workspace-on-startup nil)
    (add-hook 'sclang-mode-hook 'smartparens-mode)))

(use-package sclang-extensions
  :ensure   t
  :commands sclang-extensions-mode
  :init     (add-hook 'sclang-mode-hook 'sclang-extensions-mode))

;;; Misc language modes

(use-package make-mode
  :defer t
  :config
  (progn
    (add-to-list 'ac-modes 'makefile-mode)
    (hook-fn 'makefile-mode-hook
      (auto-complete-mode t)
      (setq indent-tabs-mode t))))

(use-package json-mode
  :ensure t
  :commands json-mode
  :mode ("\\.json$" . json-mode)
  :config
  (progn
    (add-to-list 'ac-modes 'json-mode)
    (defalias 'format-json 'beautify-json)))

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

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"          . markdown-mode)
         ("\\.[mM]arkdown$" . markdown-mode))
  :config
  (hook-fn 'markdown-mode-hook
    (buffer-face-set `(:family ,cb:serif-font :height 130))
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
  :mode ((".gitignore$" . conf-mode)
         (".gitmodules$" . conf-mode)))

;;; Git

(use-package magit
  :ensure t
  :bind   ("M-g" . magit-status)
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

    (add-hook 'magit-log-edit-mode-hook 'cb:append-buffer)

    (add-hook 'magit-mode-hook 'magit-load-config-extensions)))

(use-package git-gutter
  :ensure t
  :bind ("C-x g" . git-gutter:toggle)
  :commands (git-gutter:toggle
             git-gutter:clean
             git-gutter))

(use-package gist
  :ensure t
  :commands (gist-list
             gist-region
             gist-region-private
             gist-buffergist-buffer-private
             gist-region-or-buffer
             gist-region-or-buffer-private))

(use-package ediff
  :commands (ediff ediff-merge-files-with-ancestor)
  :init
  (progn

    (defun cb:apply-diff ()
      (let ((file ediff-merge-store-file))
        (set-buffer ediff-buffer-C)
        (write-region (point-min) (point-max) file)
        (message "Merge buffer saved in: %s" file)
        (set-buffer-modified-p nil)
        (sit-for 1)))

    (defun cb:handle-git-merge (local remote base merged)
      "Configure this emacs session for use as the git mergetool."
      (add-hook 'ediff-quit-hook 'kill-emacs)
      (add-hook 'ediff-quit-merge-hook 'cb:apply-diff)
      (ediff-merge-files-with-ancestor local remote base nil merged)))

  :config
  (progn
    (setq diff-switches "-u"
          ediff-window-setup-function 'ediff-setup-windows-plain)
    (add-hook 'ediff-startup-hook 'turn-off-evil-mode)))

(use-package cb-ediff
  :commands cb:handle-git-merge)

;;; Productivity

(use-package key-chord
  :ensure t
  :config
  (progn
    ;; Global keys.
    (key-chord-define-global "x;" 'cb:kill-current-buffer)

    ;; Paredit keys.
    (progn-after-load 'paredit
      (key-chord-define paredit-mode-map "qj" 'paredit-backward-slurp-sexp)
      (key-chord-define paredit-mode-map "qk" 'cb:paredit-forward-slurp-sexp-neatly)
      (key-chord-define paredit-mode-map "ql" 'paredit-splice-sexp-killing-backward)
      (key-chord-define paredit-mode-map "qn" 'paredit-backward-barf-sexp)
      (key-chord-define paredit-mode-map "qm" 'paredit-forward-barf-sexp))

    (key-chord-mode +1)))

(use-package scratch
  :ensure   t
  :commands scratch
  :bind     ("C-c e s" . scratch) )

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style   'forward
        uniquify-separator           "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re   "^\\*"))

(use-package org
  :ensure t
  :defer t
  :init
  (macrolet ((maybe (f) `(lambda ()
                           (unless (derived-mode-p 'org-mode)
                             (funcall ,f)))))

    ;; Use org commands in other modes.
    (add-hook 'message-mode-hook 'turn-on-orgstruct++)
    (add-hook 'message-mode-hook 'turn-on-orgtbl)
    (add-hook 'text-mode-hook (maybe 'turn-on-orgstruct++))
    (add-hook 'text-mode-hook (maybe 'turn-on-orgtbl)))
  :config
  (progn
    (setq org-catch-invisible-edits 'smart)
    (define-key org-mode-map (kbd "M-p") 'org-metaup)
    (define-key org-mode-map (kbd "M-n") 'org-metadown)))

(use-package iedit
  :ensure   t
  :commands (iedit-mode
             iedit-replace-occurrences
             iedit-done)
  :bind     ("C-<return>" . iedit-mode)
  :init
  (progn

    (defun cb:rename-symbol-in-defun (replacement)
      (interactive "sReplace in function: ")
      (iedit-mode 0)
      (iedit-replace-occurrences replacement)
      (iedit-done))

    (defun cb:rename-symbol-in-buffer (replacement)
      (interactive "sReplace in buffer: ")
      (iedit-mode)
      (iedit-replace-occurrences replacement)
      (iedit-done))

    (bind-key* "M-r" 'cb:rename-symbol-in-defun)
    (bind-key* "M-R" 'cb:rename-symbol-in-buffer)))

(use-package info-lookmore
  :commands info-lookmore-elisp-cl
  :init     (eval-after-load "info-look" '(info-lookmore-elisp-cl)))

(use-package proced
  :defer t
  :bind ("C-x p" . proced))

(use-package ack-and-a-half
  :ensure t
  :commands (ack-and-a-half-same
             ack-and-a-half-find-file
             ack-and-a-half-find-file-same))

;;; ----------------------------------------------------------------------------
;;; Misc commands

(defun cb:swap-with-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(bind-key* "C-;" 'cb:swap-with-previous-buffer)

;;; Byte compilation

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
;;; Show quote if 'fortune' is installed.

(defun fortune ()
  "Display a quotation from the 'fortune' program."
  (interactive)
  (when (executable-find "fortune")
    (message (s-trim (shell-command-to-string "fortune -s -n 250")))))

(hook-fn 'after-init-hook
  ;; Show fortune.
  (run-with-idle-timer 0.1 nil 'fortune)

  ;; Load site-file.
  (load (concat user-emacs-directory "site-file.el") t t))

;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:

;;; init.el ends here
