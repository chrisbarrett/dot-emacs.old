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
(autoload 'with-elapsed-timer "use-package")

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

(use-package cl-lib)

(use-package cb-macros
  :init
  (progn

;;;; Paths

    (define-path cb:lib-dir       "lib/" t)
    (define-path cb:lisp-dir      "lisp/" t)
    (define-path cb:src-dir       "src")
    (define-path cb:tmp-dir       "tmp/")
    (define-path cb:elpa-dir      "elpa/")
    (define-path cb:bin-dir       "bin/")
    (define-path cb:etc-dir       "etc/")
    (define-path cb:yasnippet-dir "snippets/")
    (define-path cb:backups-dir   "backups/")
    (define-path cb:autosaves-dir "tmp/autosaves/")))

;;;; Initial Configuration

(with-elapsed-timer "Loading base config"

(hook-fn 'text-mode-hook
  "Use a sans-serif font for text-mode."
  (when (equal major-mode 'text-mode)
    (buffer-face-set `(:family ,(sans-serif-font) :height 120))))

(hook-fn 'Info-mode-hook
  (buffer-face-set `(:family ,(serif-font) :height 140)))

;; Use the version of emacs in /src for info and source.
(setq source-directory (format "%s/emacs-%s.%s" cb:src-dir
                               emacs-major-version
                               emacs-minor-version))
(setenv "INFOPATH" (concat source-directory "/info/"))

;;; Basic configuration.

(use-package personal-config)

(setq
 redisplay-dont-pause         t
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
 system-uses-terminfo         nil
 bookmark-default-file        (concat cb:tmp-dir "bookmarks")
 )
(setq-default
 tab-width                    4
 indent-tabs-mode             nil
 fill-column                  75)
(icomplete-mode +1)

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
;;; Rebind to C-c k k ("kill") to prevent accidentally exiting when
;;; using Org bindings.
(bind-key* "C-x C-c" (command (message "Type <C-c k e> to exit Emacs")))
(bind-key* "C-c k k" 'cb:exit-emacs-dwim)
(bind-key* "C-c k e" 'cb:exit-emacs)

(autoload 'ido-yes-or-no-p "ido-yes-or-no")

(defun cb:exit-emacs ()
  (interactive)
  (when (ido-yes-or-no-p "Kill Emacs? ")
    (save-buffers-kill-emacs)))

(defun cb:exit-emacs-dwim ()
  (interactive)
  (when (ido-yes-or-no-p "Kill Emacs? ")
    (if (daemonp)
        (server-save-buffers-kill-terminal nil)
      (save-buffers-kill-emacs))))

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

(bind-key "C-c e e" 'toggle-debug-on-error)

;;; Editing Advice

(defun* sudo-edit (&optional (file (buffer-file-name)))
  "Edit FILE with sudo if permissions require it."
  (interactive)
  (when file
    (cond
     ((directory-p file)
      (error "%s is a directory" file))

     ((file-writable-p file)
      (error "%s: sudo editing not needed" file))

     ;; Prompt user whether to escalate. Ensure the tramp connection is
     ;; cleaned up afterwards.
     ((and (ido-yes-or-no-p "Edit file with sudo?  ")
           (find-alternate-file (concat "/sudo:root@localhost:" file)))
      (add-hook 'kill-buffer-hook 'tramp-cleanup-this-connection nil t)))))

(bind-key* "C-x e" 'sudo-edit)

(hook-fn 'find-file-hook
  "Offer to create a file with sudo if necessary."
  (let ((dir (file-name-directory (buffer-file-name))))
    (when (or (and (not (file-writable-p (buffer-file-name)))
                   (file-exists-p (buffer-file-name)))

              (and dir
                   (file-exists-p dir)
                   (not (file-writable-p dir))))
      (sudo-edit))))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Suppress \"Active processes exist\" query when exiting Emacs."
  (flet ((process-list ()))
    ad-do-it))

(hook-fn 'kill-emacs-hook
  "Ensure tramp resources are released on exit."
  (when (fboundp 'tramp-cleanup-all-buffers)
    (tramp-cleanup-all-buffers)))

(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab activate)
  "Fix `whitespace-cleanup' bug when using `indent-tabs-mode'."
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

;;; Other advice

(declare-modal-view package-list-packages)

(declare-modal-executor init-dot-el
  :command (find-file (concat user-emacs-directory "init.el"))
  :bind    "M-I")

)

;;;; Typefaces

(with-elapsed-timer "Loading typefaces"

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

;;; Use monospace font by default.
(set-frame-font (format "%s 11" (monospace-font)) t)
(hook-fn 'after-make-frame-functions
  (set-frame-font (format "%s 11" (monospace-font)) t
                  (list (car (frame-list)))))

)

;;;; Modeline

(with-elapsed-timer "Loading modeline"

(defun* cb:vc-state->letter (&optional (file (buffer-file-name)))
  "Return a single letter to represent the current version-control status."
  (case (ignore-errors (vc-state file))
    ((up-to-date)           " ")
    ((edited)               (propertize "M" 'face '(:foreground "red")))
    ((needs-merge conflict) (propertize "!" 'face '(:foreground "red")))
    ((added)                (propertize "A" 'face '(:foreground "green")))
    ((removed)              (propertize "D" 'face '(:foreground "red")))
    ((ignored)              (propertize "-" 'face 'modeline-vc-unknown-face))
    (t                      (propertize "?" 'face 'modeline-vc-unknown-face))))

(defun* cb:vc-file-uptodate? (&optional (file (buffer-file-name)))
  "Non-nil if FILE is up-to-date."
  (ignore-errors (equal 'up-to-date (vc-state file))))

(defun* cb:shorten-directory (dir &optional (max-length 30))
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

(defun* cb:propertize-file-directory
    (&optional (filepath (file-name-directory (buffer-file-name))))
  "Separate tramp info from the given filepath."
  (flet ((face
          (str face)
          (propertize str 'face face)))

    (destructuring-bind (&optional method user host file &rest _)
        (mapcar 'identity (ignore-errors
                            (tramp-dissect-file-name filepath)))
      (concat
       (when host
         (concat
          (face "/" 'mode-line-tramp-separator)
          (face method 'mode-line-tramp-method)
          (face ":" 'mode-line-tramp-separator)
          (face user 'mode-line-tramp-user)
          (face "@" 'mode-line-tramp-separator)
          host
          (face ":" 'mode-line-tramp-separator)))
       (face (cb:shorten-directory (or file filepath)) 'mode-line-directory)))))

(defface mode-line-tramp-separator
  '((((type graphic) (background dark))
     (:foreground "gray45"))
    (((type graphic) (background light))
     (:foreground "gray80"))
    (t
     (:inherit 'mode-line)))
  "Face for separator characters in modeline."
  :group 'modeline)

(defface mode-line-tramp-method
  '((t (:inherit 'mode-line)))
  "Face for tramp method in modeline."
  :group 'modeline)

(defface mode-line-tramp-user
  '((((type graphic))
     (:foreground "VioletRed3"))
    (t (:inherit 'mode-line)))
  "Face for tramp user indicator in modeline."
  :group 'modeline)

;; Extra mode line faces

(defface modeline-vc-unknown-face
  '((((type graphic) (background dark))
     (:foreground "yellow"))
    (((type graphic) (background light))
     (:foreground "blue"))
    (t
     (:inherit 'mode-line)))
  "Face for unknown vc file status."
  :group 'modeline)

(defface mode-line-read-only
  '((((type graphic))
     (:foreground "#4271ae"
      :box '(:line-width 2 :color "#4271ae")))
    (t (:inherit 'mode-line)))
  "Face for readonly indicator."
  :group 'modeline)

(defface mode-line-modified
  '((((type graphic))
     (:foreground "#c82829"))
    (t
     (:inherit 'mode-line)))
  "Face for modified indicator."
  :group 'modeline)

(defface mode-line-directory
  '((((type graphic) (background dark))
     (:foreground "gray60"))
    (((type graphic) (background light))
     (:foreground "gray70"))
    (t
     (:inherit 'mode-line)))
  "Face for the directory component of the current filename."
  :group 'modeline)

(defface mode-line-filename
  '((((type graphic) (background dark))
     (:foreground "#eab700" :weight bold))
    (((type graphic) (background light))
     (:foreground "gray40" :weight bold))
    (t
     (:inherit 'mode-line)))
  "Face for the name component of the current filename."
  :group 'modeline)

(defface mode-line-position
  `((((type graphic) (background dark))
     (:family ,(monospace-font)
      :height 100
      :foreground "gray60"))
    (((type graphic) (background light))
     (:family ,(monospace-font)
      :height 100
      :foreground "gray50"))
    (t
     (:inherit 'mode-line)))
  "Face for the position indicators."
  :group 'modeline)

(defface mode-line-mode
  '((((type graphic) (background dark))
     (:foreground "gray70"))
    (((type graphic) (background light))
     (:foreground "gray40"))
    (t
     (:inherit 'mode-line)))
  "Face for the current major mode indicator."
  :group 'modeline)

(defface mode-line-minor-mode
  '((((type graphic) (background dark))
     (:foreground "gray40" :height 110))
    (((type graphic) (background light))
     (:foreground "gray70" :height 110))
    (t (:inherit 'mode-line-mode)))
  "Face for the current minor mode indicators."
  :group 'modeline)

(defface mode-line-process
  '((((type graphic))
     (:foreground "#718c00"))
    (t
     (:inherit 'mode-line)))
  "Face for the current process."
  :group 'modeline)

(defface mode-line-80col
  '((((type graphic) (background dark))
     (:foreground "#eab700"))
    (((type graphic) (background light))
     (:foreground "#b58900"))
    (t
     (:inherit 'mode-line-position)))
  "Face for the warning when point is past column 80."
  :group 'modeline)

(autoload 'vc-git-root "vc-git")

(setq-default
 mode-line-format
 `(
   ;; --------------------------------------------------------------------------
   ;; Line and column number.
   (:propertize " %4l:" face mode-line-position)
   (:eval
    ;; Warn if over 80 columns.
    (propertize "%3c" 'face
                (if (>= (current-column) 80)
                    'mode-line-80col
                  'mode-line-position)))
   " "
   ;; --------------------------------------------------------------------------
   ;; File status.
   (:eval
    (let ((blank "    "))
      (cond
       ;; Do not show status for special buffers.
       ((and (s-starts-with? "*" (buffer-name))
             (not (buffer-file-name)))
        blank)

       ;; Show read-only indicator.
       (buffer-read-only
        (propertize " RO " 'face 'mode-line-read-only))

       ;; Show modified and vc status.
       (t
        (format " %s%s "
                (if (ignore-errors (vc-git-root (buffer-file-name)))
                    (cb:vc-state->letter)
                  " ")
                (if (buffer-modified-p)
                    (propertize "*" 'face 'mode-line-modified)
                  " "))))))
   " "
   ;; --------------------------------------------------------------------------
   ;; Buffer name and path.
   (:eval (if (buffer-file-name) (cb:propertize-file-directory) ""))
   (:propertize "%b" face mode-line-filename)

   ;; --------------------------------------------------------------------------
   ;; Narrowing
   " %n "

   ;; --------------------------------------------------------------------------
   ;; Mode details.

   ;; Major mode.
   " %["
   (:propertize mode-name
                face mode-line-mode)
   "%] "

   ;; ERT status.
   (:eval (when (cb:truthy? 'ert-modeline-mode)
            (set-face-bold 'ertml-failing-face t)
            (let ((s (s-trim ertml--status-text)))
              (if (s-matches? (rx digit) s)
                  (propertize s 'face 'ertml-failing-face)
                (propertize s 'face 'bold)))))

   ;; Minor modes.
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode))
   (:propertize mode-line-process
                face mode-line-process)
   (global-mode-string global-mode-string))))

;;; ============================================================================

;;;; Common hooks

(with-elapsed-timer "Configuring basic hooks"

(defun cb:next-dwim ()
  "Perform a context-sensitive 'next' action."
  (interactive)
  (cond
   ((cb:truthy? 'edebug-active)
    (edebug-next-mode))
   (t
    (next-error))))

(hook-fn 'prog-mode-hook
  "Generic programming mode configuration."

  ;; Error navigation keybindings.
  (local-set-key (kbd "M-N") 'cb:next-dwim)
  (local-set-key (kbd "M-P") 'previous-error)

  ;; Highlight special comments.
  (font-lock-add-keywords
   major-mode '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                 1 font-lock-warning-face t))))

(hook-fn 'Buffer-menu-mode-hook
  "Buffer menu only shows files on disk."
  (Buffer-menu-toggle-files-only +1))

(hook-fn 'text-mode-hook
  (unless (derived-mode-p 'sgml-mode 'nxhtml-mode)
    (turn-on-auto-fill)))

(defalias 'yes-or-no-p 'y-or-n-p)

)

;;;; Mode groups

(with-elapsed-timer "Configuring mode groups"

(defmacro define-combined-hook (name hooks)
  "Create a hook bound as NAME that is run after each hook in HOOKS."
  (declare (indent 1))
  `(progn
     ;; Define combined hook.
     (defvar ,name nil "Auto-generated combined hook.")
     ;; Add combined hook to each of the given hooks.
     (--each ,hooks
       (hook-fn it
         (run-hooks ',name)))))

(defmacro define-mode-group (name modes)
  "Create an ad-hoc relationship between language modes.
* Creates a special var with NAME to contain the grouping.
* Declares a hook NAME-hook that runs after any of MODES are initialized."
  (declare (indent 1))
  (let ((hook (intern (format "%s-hook" name))))
    `(progn
       ;; Define modes variable.
       (defconst ,name ,modes "Auto-generated variable for language grouping.")
       ;; Create a combined hook for MODES.
       (define-combined-hook ,hook
         (--map (intern (concat (symbol-name it) "-hook"))
                ,modes)))))

(define-mode-group cb:scheme-modes
  '(scheme-mode
    inferior-scheme-mode
    geiser-repl-mode
    geiser-mode))

(define-mode-group cb:clojure-modes
  '(clojure-mode
    clojurescript-mode))

(define-mode-group cb:elisp-modes
  '(emacs-lisp-mode
    ielm-mode))

(define-mode-group cb:slime-modes
  '(slime-mode
    slime-repl-mode))

(define-mode-group cb:lisp-modes
  `(,@cb:scheme-modes
    ,@cb:clojure-modes
    ,@cb:elisp-modes
    ,@cb:slime-modes
    common-lisp-mode
    inferior-lisp-mode
    lisp-mode
    repl-mode))

(define-mode-group cb:haskell-modes
  '(haskell-mode
    inferior-haskell-mode
    haskell-interactive-mode
    haskell-c-mode
    haskell-cabal-mode))

(define-mode-group cb:python-modes
  '(python-mode
    inferior-python-mode))

(define-mode-group cb:ruby-modes
  '(inf-ruby-mode
    ruby-mode))

(define-mode-group cb:rails-modes
  `(,@cb:ruby-modes
    erb-mode))

(define-mode-group cb:xml-modes
  '(sgml-mode
    nxml-mode))

(define-mode-group cb:org-minor-modes
  '(orgtbl-mode
    orgstruct-mode
    orgstruct++-mode))

(define-mode-group cb:prompt-modes
  '(comint-mode
    inf-ruby-mode
    inferior-python-mode
    ielm-mode
    erc-mode
    slime-repl-mode
    inferior-scheme-mode
    inferior-haskell-mode
    sclang-post-buffer-mode))

(defun cb:clear-scrollback ()
  "Erase all but the last line of the current buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (last-line (save-excursion
                     (goto-char (point-max))
                     (forward-line -1)
                     (line-end-position))))
    (delete-region (point-min) last-line)
    (goto-char (point-max))))

(hook-fn 'cb:prompt-modes-hook
  (local-set-key (kbd "C-a") 'move-beginning-of-line)
  (local-set-key (kbd "C-e") 'move-end-of-line)
  (local-set-key (kbd "C-l") 'cb:clear-scrollback)
  (local-set-key (kbd "M->") 'cb:append-buffer)
  (cb:append-buffer))

)

;;; ----------------------------------------------------------------------------

;;; Forward declaration.
(defvar ac-modes nil)

(defun cb:truthy? (sym)
  "Test whether SYM is bound and non-nil."
  (and (boundp sym) (eval sym)))

(autoload 'woman-file-name-all-completions "woman")

(defun get-manpage (candidate)
  "Show the manpage for CANDIDATE."
  (let ((wfiles (mapcar 'car (woman-file-name-all-completions candidate))))
    (condition-case err
        (if (> (length wfiles) 1)
            (woman-find-file
             (helm-comp-read
              "ManFile: " wfiles :must-match t))
          (woman candidate))
      ;; If woman is unable to format correctly
      ;; use man instead.
      (error
       (kill-buffer)
       (Man-getpage-in-background candidate)))))

(defun get-elisp-doc (sym)
  "Find the appropriate documentation for SYM."
  (cond
   ((symbol-function sym) (describe-function sym))
   ((facep sym)           (describe-face sym))
   ((boundp sym)          (describe-variable sym))
   (t                     (user-error "No documentation available"))))

(defun* get-documentation (&optional (candidate (thing-at-point 'symbol)))
  "Get documentation for CANDIDATE."
  (interactive)
  (condition-case _
    (cond
     ((apply 'derived-mode-p cb:elisp-modes)
      (get-elisp-doc (intern candidate)))
     (t
      (get-manpage candidate)))
    (error
     (user-error "No documentation available"))))

(use-package simple
  :diminish (visual-line-mode
             global-visual-line-mode
             auto-fill-mode)
  :init
  (add-hook 'text-mode-hook 'visual-line-mode))

(use-package exec-path-from-shell
  :ensure t
  :defer  t
  :if     (or (daemonp)
              (and (equal system-type 'darwin) (window-system)))
  :init   (hook-fn 'after-init-hook (require 'exec-path-from-shell))
  :config (exec-path-from-shell-initialize))

;;;; OS X

(with-elapsed-timer "Configuring OS X"

;; Enable mouse support in terminal.
(use-package mouse
  :if (not (display-graphic-p))
  :config
  (progn
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    (setq mouse-sel-mode t)

    (when (equal system-type 'darwin)
     (global-set-key [mouse-4] (command (scroll-down 1)))
     (global-set-key [mouse-5] (command (scroll-up 1))))))

(when (equal system-type 'darwin)

  ;; Set terminfo so ansi-term displays shells correctly.

  (let ((terminfo (expand-file-name "~/.terminfo")))
    (unless (file-exists-p terminfo)
      (start-process
       "tic" " tic" "tic"
       "-o" terminfo
       "/Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti")))

  ;; Use system clipboard.

  (unless window-system

    (defun cb:osx-paste ()
      (shell-command-to-string "pbpaste"))

    (defun cb:osx-copy (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq interprogram-cut-function   'cb:osx-copy
          interprogram-paste-function 'cb:osx-paste)))

)

;;;; Helm

(use-package projectile
  :ensure   t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-known-projects-file
          (concat cb:tmp-dir "projectile-bookmarks.eld"))
    (projectile-global-mode)
    (defadvice find-tag (before set-tags-directory activate)
      "Ensure the TAGS path is set before searching for tags."
      (setq tags-file-name (concat (projectile-project-root) "TAGS")))))

(use-package helm
  :ensure t
  :defer  t
  :idle   (require 'helm)
  :init
  (progn
     (after 'helm
      (define-key helm-map (kbd "C-[") 'helm-keyboard-quit))

    (bind-key* "M-a" 'helm-apropos)
    (bind-key* "M-b" 'helm-buffers-list)
    (bind-key* "C-x C-b" 'helm-buffers-list)
    (bind-key* "M-j" 'helm-mini)
    (bind-key* "M-i" 'helm-imenu)
    (bind-key* "M-f" 'helm-etags-select)
    (bind-key* "M-m" 'helm-man-woman)
    (bind-key* "M-w" 'helm-w3m-bookmarks)
    (bind-key* "M-k" 'helm-show-kill-ring)))

(use-package helm-projectile
  :ensure t
  :commands helm-projectile
  :idle (require 'helm-projectile)
  :init
  (progn
      (defun cb:helm-dwim ()
      "Show helm-projectile, failling back to helm-mini if not in a project."
      (interactive)
      (if (projectile-project-p)
          (helm-projectile)
        (helm-mini)))

    (bind-key* "C-j" 'cb:helm-dwim)))

;;;; Ido

(defmacro declare-ido-wrapper (command)
  "Make COMMAND use ido for file and directory completions."
  `(defadvice ,command (around read-with-ido activate)
     (flet
         ((read-directory-name
           (&rest args) (apply 'ido-read-directory-name args))
          (read-file-name
           (&rest args) (apply 'ido-read-file-name args))
          (read-buffer
           (&rest args) (apply 'ido-read-buffer)))
       ad-do-it)))

(use-package ido
  :ensure t
  :idle   (require 'ido)
  :commands
  (ido-mode
   ido-find-file
   ido-switch-buffer
   ido-switch-buffer-other-window
   ido-display-buffer
   ido-kill-buffer
   ido-insert-buffer
   ido-switch-buffer-other-frame
   ido-find-file-in-dir
   ido-find-file-other-window
   ido-find-alternate-file
   ido-find-file-read-only
   ido-find-file-read-only-other-window
   ido-find-file-read-only-other-frame
   ido-display-file
   ido-find-file-other-frame
   ido-write-file
   ido-insert-file
   ido-dired
   ido-read-buffer
   ido-read-file-name
   ido-read-directory-name
   ido-completing-read)
  :init
  (progn
    (bind-key "C-x C-f" 'ido-find-file)
    (bind-key "C-x d"   'ido-dired)
    (bind-key "C-x i"   'ido-insert-file)
    (bind-key "C-x C-w" 'ido-write-file)
    (bind-key "C-x k"   'ido-kill-buffer)
    (bind-key "C-x b"   'ido-switch-buffer))
  :config
  (progn
    (setq
     ido-enable-prefix            nil
     ido-save-directory-list-file (concat cb:tmp-dir "ido.last")
     ido-enable-flex-matching     t
     ido-create-new-buffer        'always
     ido-use-filename-at-point    'guess
     ido-max-prospects            10
     ido-default-file-method      'selected-window)
    (add-to-list 'ido-ignore-buffers "\\*helm.*")
    (add-to-list 'ido-ignore-buffers "\\*Minibuf.*")
    (add-to-list 'ido-ignore-files "\\.swp")
    (add-to-list 'ido-ignore-files "\\.DS_Store")

    (hook-fn 'ido-setup-hook
      ;; Typing ~ resets ido prompt to home directory.
      (define-key ido-common-completion-map
        (kbd "~")
        (command
         (if (looking-back "/")
             (insert "~/")
           (call-interactively 'self-insert-command)))))

    (ido-mode +1)))

(use-package ido-hacks
  :ensure t
  :commands ido-hacks-mode
  :init (after 'ido (ido-hacks-mode +1)))

(use-package ido-ubiquitous
  :ensure t
  :commands ido-ubiquitous-mode
  :init
  (after 'ido
    (macrolet
        ((use-new-completing-read
          (cmd package)
          `(eval-after-load ,package
             '(defadvice ,cmd (around ido-ubiquitous-new activate)
                (let ((ido-ubiquitous-enable-compatibility nil))
                  ad-do-it)))))

      (use-new-completing-read yas/expand 'yasnippet)
      (use-new-completing-read yas/visit-snippet-file 'yasnippet))

    (ido-mode +1)
    (ido-ubiquitous-mode +1)))

(use-package ido-yes-or-no
  :ensure t
  :commands ido-yes-or-no-mode
  :init (after 'ido (ido-yes-or-no-mode +1)))

(use-package ido-better-flex
  :ensure t
  :commands ido-better-flex/enable
  :init (after 'ido (ido-better-flex/enable)))

(use-package ido-speed-hack
  :defer t
  :init (after 'ido (require 'ido-speed-hack)))

(use-package imenu
  :commands imenu
  :init
  (hook-fn 'emacs-lisp-mode-hook
    "Display section headings."
    (setq imenu-prev-index-position-function nil)
    (add-to-list 'imenu-generic-expression
                 `("SECTION"
                   ;; Match sections.
                   ,(rx bol ";;;;" (+ space) (group (+ nonl )))
                   1) t)))

(use-package smex
  :ensure t
  :idle   (require 'smex)
  :commands
  (smex
   smex-major-mode-commands)
  :init
  (progn
    (bind-key* "M-X" 'smex-major-mode-commands)
    (bind-key* "M-x" 'smex))
  :config (smex-initialize))

;;;; Buffer/window management

(use-package workgroups
  :ensure t
  :defer  t
  :idle   (require 'workgroups)
  :bind
  (("s-1" . wg-switch-to-index-0)
   ("s-2" . wg-switch-to-index-1)
   ("s-3" . wg-switch-to-index-2)
   ("s-4" . wg-switch-to-index-3)
   ("s-5" . wg-switch-to-index-4))
  :diminish workgroups-mode
  :commands workgroups-mode
  :config
  (progn
    (setq
     wg-display-current-workgroup-left-decor "["
     wg-display-current-workgroup-right-decor "]"
     wg-morph-vsteps 6
     wg-prefix-key (kbd "C-c w"))
    (wg-set-prefix-key)

    (defadvice wg-mode-line-add-display (around wg-suppress-error activate)
      "Ignore errors in modeline display function caused by custom modeline."
      (ignore-errors ad-do-it))))

(use-package popwin
  :ensure t
  :commands popwin-mode
  :init
  (hook-fn 'window-configuration-change-hook
    (unless (cb:truthy? 'popwin-mode)
      (popwin-mode +1)))
  :config
  (progn
    (hook-fn 'popwin:after-popup-hook
      "Quit popups with Q"
      (when (fboundp 'evil-define-key)
        (evil-local-set-key 'normal "q" (command (quit-window t)))))

    (setq display-buffer-function 'popwin:display-buffer
          popwin:special-display-config
          '(("*Help*"  :height 30 :stick t)
            ("*Completions*" :noselect t)
            ("*Shell Command Output*")
            ("*compilation*" :noselect t)
            ("*Messages*" :height 30)
            ("*Directory*")
            ("*Org Note*")
            ("*Occur*" :noselect t)
            ("\\*Slime Description.*" :noselect t :regexp t :height 30)
            ("*magit-commit*" :noselect t :height 40 :width 80)
            ("*magit-diff*" :height 40 :width 80)
            ("*magit-edit-log*" :noselect t :height 15 :width 80)
            ("\\*Slime Inspector.*" :regexp t :height 30)
            ("*Ido Completions*" :noselect t :height 30)
            ("*eshell*" :height 30)
            ("*shell*" :height 30)
            (".*overtone.log" :regexp t :height 30)
            ("*gists*" :height 30)
            ("*sldb.*":regexp t :height 30)))))

(use-package transpose-frame
  :bind
  (("C-x t" . transpose-frame)
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
  :commands winner-mode
  :init (hook-fn 'window-configuration-change-hook
          (unless (cb:truthy? 'winner-mode)
            (winner-mode +1))))

(use-package window-number
  :ensure t
  :defer  t
  :commands window-number-meta-mode
  :init (hook-fn 'window-configuration-change-hook (window-number-meta-mode +1)))

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
   goto-first-occurence)

  :init
  (progn
    (setq cb:kill-buffer-ignored-list
          '("*scratch*" "*Messages*" "*GROUP*"
            "*shell*" "*eshell*" "*ansi-term*"))
    (add-hook 'find-file-hook 'hide-dos-eol)
    (bind-key "C-c k b" 'clean-buffers)
    (bind-key "C-<up>" 'move-line-up)
    (bind-key "C-<down>" 'move-line-down)
    (define-key prog-mode-map (kbd "M-q") 'indent-dwim))

  :config
  (defadvice rotate-buffers (after select-largest-window activate)
    "Switch to the largest window if using a 2-up window configuration."
    (when (= 2 (length (window-list)))
      (select-largest-window))))

;;;; Backups & State

(use-package saveplace
  :init
  (progn
    (add-hook 'server-visit-hook 'save-place-find-file-hook)
    (add-hook 'server-done-hook  'save-place-kill-emacs-hook)
    (setq save-place-file (concat cb:tmp-dir "saved-places"))
    (setq-default saveplace t)))

(use-package recentf
  :defer t
  :idle  (require 'recentf)
  :config
  (defadvice recentf-cleanup (around hide-messages activate)
    (flet ((message (&rest args)))
      ad-do-it))
  :init
  (progn
    (hook-fn 'find-file-hook (require 'recentf))
    (setq
     recentf-save-file       (concat cb:tmp-dir "recentf")
     recentf-auto-cleanup    5
     recentf-keep            '(file-remote-p file-readable-p)
     recentf-max-saved-items 100
     recentf-max-menu-items  25
     recentf-exclude '(".newsrc"
                       "tmp"
                       "^/?sudo"
                       "Emacs.app"
                       "-autoloads.el"
                       "recentf"
                       ".ido.last"
                       "TAGS"
                       ".gz"))))

(use-package backup-dir
  :defer t
  :idle  (require 'backup-dir)
  :init
  (hook-fn 'find-file-hook (require 'backup-dir))
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
  :init
  (progn
    (setq
     savehist-additional-variables '(search ring regexp-search-ring)
     savehist-autosave-interval    60
     savehist-file                 (concat cb:tmp-dir "savehist"))
    (savehist-mode +1)))

(use-package undo-tree
  :ensure   t
  :defer    t
  :idle     (require 'undo-tree)
  :bind     ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :init
  (hook-fn 'find-file-hook (require 'undo-tree))
  :config
  (global-undo-tree-mode +1))

;;;; Cosmetic

(use-package highlight
  :ensure t
  :defer t)

(use-package hl-line
  :if (or (daemonp) (display-graphic-p))
  :config (global-hl-line-mode t))

(use-package fringe
  :idle     (require 'fringe)
  :commands fringe-mode
  :config   (fringe-mode '(2 . 0)))

(use-package ansi-color
  :defer t
  :init
  (progn
    (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on))
  :config
  (defadvice display-message-or-buffer (before ansi-color activate)
    "Process ANSI color codes in shell output."
    (let ((buf (ad-get-arg 0)))
      (and (bufferp buf)
           (string= (buffer-name buf) "*Shell Command Output*")
           (with-current-buffer buf
             (ansi-color-apply-on-region (point-min) (point-max)))))))

;;;; Navigation

(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-L" . ace-jump-line-mode)
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

;;;; Web

(use-package smtpmail
  :commands smtpmail-send-it
  :init
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it))

(use-package gnus
  :commands gnus
  :defer t
  :config
  (setq gnus-select-method '(nnml "mail")))

(use-package bbdb
  :ensure t
  :defer  t
  :init
  (progn
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
    (add-hook 'message-mode-hook 'bbdb-insinuate-mail)
    (setq bbdb-file (concat user-dropbox-directory ".bbdb")))
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
  :commands
  (bbdb-vcard-import-file
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
  :commands
  (w3m
   w3m-find-file
   w3m-browse-url)
  :init
  (progn
    (setq browse-url-browser-function 'w3m-browse-url)

    (declare-modal-executor w3m
      :command w3m
      :bind "M-W"
      :restore-bindings '("M-W" "M-E"))

    (defun cb:find-window-with-mode (mode)
      "Find the first window whose buffer is in major-mode MODE."
      (get-window-with-predicate
       (lambda (w) (with-current-buffer (window-buffer w)
                     (equal mode major-mode)))))

    (defun cb:w3m-browse-url-as-help (url)
      "Browse the given URL in a help window."
      (interactive
       (list
        (read-string "Go to URL: "
                     (thing-at-point-url-at-point)
                     t)))
      (with-window-restore
        (let ((win (or (cb:find-window-with-mode 'w3m-mode) (split-window))))
          (select-window win)
          (w3m-browse-url url))
        (local-set-key (kbd "q") (command (restore)))))

    (defun cb:w3m-browse-dwim (url)
      "Browse to URL, ensuring it begins with http:// as reqiured by w3m."
      (interactive
       (list
        (read-string "Go to URL: "
                     (thing-at-point-url-at-point)
                     t)))
      (with-window-restore
        (w3m-browse-url
         (if (s-starts-with? "http://" url)
             url
           (concat "http://" url)))
        (local-set-key (kbd "q") (command (restore)))))

    (bind-key* "M-e" 'cb:w3m-browse-dwim)
    )
  :config
  (hook-fn 'w3m-mode-hook
    (buffer-face-set
     `(:family ,(serif-font) :height 130))))

(use-package erc
  :defer t
  :config
  (progn
    (erc-autojoin-mode +1)
    (erc-track-mode +1)
    (setq
     erc-hide-list
     '("JOIN" "PART" "QUIT" "NICK")

     erc-prompt
     (lambda () (format "%s>" (erc-current-nick)))

     erc-track-exclude-types
     '("JOIN" "NICK" "PART" "QUIT" "MODE"
       "324" "329" "332" "333" "353" "477"))))

;;;; Themes

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package ir-black-theme
  :ensure t
  :defer t)

(use-package cb-colour
  :if (or (daemonp) (window-system))
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
      (error (solarized-light)))

    (hook-fn 'cb:color-theme-changed-hook
      (set-face-font 'default (format "%s 11" (monospace-font)))
      (with-temp-buffer
        (insert (prin1-to-string (list (car args))))
        (write-file cb:last-theme))
      (message nil))))

;;;; Vim & Evil

(use-package evil
  :ensure t
  :commands evil-mode
  :init (evil-mode +1)
  :config
  (progn
    (use-package cb-evil)

    (defun cb:append-buffer ()
      "Enter insertion mode at the end of the current buffer."
      (interactive)
      (goto-char (point-max))
      (when (fboundp 'evil-append-line)
        (evil-append-line 1)))

    (defun evil-undefine ()
      (interactive)
      (let (evil-mode-map-alist)
        (call-interactively (key-binding (this-command-keys)))))

    (define-key evil-normal-state-map (kbd "M-z") 'evil-emacs-state)
    (define-key evil-emacs-state-map  (kbd "M-z") 'evil-normal-state)
    (define-key evil-normal-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
    (define-key evil-insert-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-visual-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-normal-state-map (kbd "K")   'get-documentation)

    (after "man"
      (evil-declare-key 'normal Man-mode-map (kbd "q") 'Man-kill))

    (after "org"
      (evil-define-key 'normal org-mode-map (kbd "SPC")
        'org-cycle)
      (evil-define-key 'normal org-mode-map (kbd "z m")
        (command (org-global-cycle 1)))
      (evil-define-key 'normal org-mode-map (kbd "z r")
        (command (org-global-cycle 0))))

    (after "undo-tree"
      ;; Ensure undo-tree commands are remapped. The referenced keymap in
      ;; evil-integration is incorrect.
      (define-key undo-tree-visualizer-mode-map [remap evil-backward-char]
        'undo-tree-visualize-switch-branch-left)
      (define-key undo-tree-visualizer-mode-map [remap evil-forward-char]
        'undo-tree-visualize-switch-branch-right)
      (define-key undo-tree-visualizer-mode-map [remap evil-next-line]
        'undo-tree-visualize-redo)
      (define-key undo-tree-visualizer-mode-map [remap evil-previous-line]
        'undo-tree-visualize-undo))

    (evil-global-set-key 'insert (kbd "S-TAB") 'tab-to-tab-stop)

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

(use-package surround
  :ensure t
  :defer  t
  :idle   (require 'surround)
  :init   (after 'evil (require 'surround))
  :config
  (progn
    (global-surround-mode +1)
    (hook-fn 'prog-mode-hook
      (push '(?\( . ("(" . ")")) surround-pairs-alist)
      (push '(?\[ . ("[" . "]")) surround-pairs-alist)
      (push '(?< . ("<" . ">")) surround-pairs-alist))
    (hook-fn 'cb:lisp-modes-hook
      (push '(?\{ . ("{" . "}")) surround-pairs-alist))))

(use-package evil-numbers
  :ensure t
  :commands
  (evil-numbers/dec-at-pt
   evil-numbers/inc-at-pt)
  :config
  (progn
    (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
    (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)))

;;;; Shells

(use-package eshell
  :commands eshell
  :config
  (setq eshell-prompt-function
        (lambda ()
          (format "%s\n%s"
                  (abbreviate-file-name (eshell/pwd))
                  (if (= (user-uid) 0) " # " " % ")))))

(use-package term
  :bind ("M-T" . cb:term-cycle)
  :defer t

  :init
  (defun cb:term-cycle ()
    "Cycle through various terminal window states."
    (interactive)
    (cond
     ;; If terminal is maximized, restore previous window config.
     ((and (derived-mode-p 'term-mode)
           (equal 1 (length (window-list))))
      (bury-buffer)
      (jump-to-register :term-fullscreen))

     ;; If we're looking at the terminal, maximize it.
     ((derived-mode-p 'term-mode)
      (delete-other-windows))

     ;; Otherwise show the terminal.
     (t
      ;; First we hide the term window if it's visible. Then we save this
      ;; configuration to a register so that it can be restored at for
      ;; later positions in the cycle.
      (-when-let (win (--first (equal (buffer-name (window-buffer it))
                                      "*ansi-term*")
                               (window-list)))
        (delete-window win))
      (window-configuration-to-register :term-fullscreen)
      (let* ((buf (get-buffer "*ansi-term*"))
             (proc (get-buffer-process buf)))
        (unless proc
          (kill-buffer buf))
        (if (and buf proc)
            (switch-to-buffer-other-window buf)
          (ansi-term (executable-find "zsh")))))))

  :config
  (progn
    (add-to-list 'ac-modes 'term-mode)

    (defun cb:ansi-term-paste ()
      "Correct paste handling in ansi-term."
      (interactive)
      (process-send-string
       (get-buffer-process (current-buffer))
       (current-kill 0)))

    (hook-fn 'term-mode-hook
      (setq ac-sources '(ac-source-filename))

      (local-set-key (kbd "s-v") 'cb:ansi-term-paste)

      (when (cb:truthy? 'evil-mode)
        (evil-define-key 'normal term-mode-map "p" 'cb:ansi-term-paste))

      (define-key term-raw-map (kbd "M-T") 'cb:term-cycle)
      ;; Yasnippet causes tab-completion to fail.
      (yas-minor-mode -1))

    (hook-fn 'window-configuration-change-hook
      "Change process window size."
      (when (and (derived-mode-p 'comint-mode 'term-mode)
                 (get-buffer-process (current-buffer)))
        (set-process-window-size (get-buffer-process (current-buffer))
                                 (window-height)
                                 (window-width))))

    (defadvice ansi-term (after move-to-end-of-buffer activate)
      "Move to the end of the shell buffer and enter insertion state."
      (cb:append-buffer))))

(use-package sh-script
  :mode (("\\.zsh" . shell-script-mode )))

;;;; Completion

(use-package auto-complete
  :ensure   t
  :idle     (require 'auto-complete)
  :diminish auto-complete-mode
  :commands
  (global-auto-complete-mode
   auto-complete-mode)

  :init
  (progn
    (after 'auto-complete (global-auto-complete-mode +1))
    (add-hook 'find-file-hook 'auto-complete-mode))

  :config
  (progn
    ;; Enable for everything except text mode.


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

(use-package yasnippet
  :ensure t
  :idle   (require 'yasnippet)
  :diminish yas-minor-mode
  :commands
  (yas-global-mode
   yas-minor-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'sgml-mode-hook 'yas-minor-mode))
  :config
  (progn
    (setq
     yas-prompt-functions'(yas-ido-prompt)
     yas/trigger-key (kbd "RET"))
    (add-to-list 'yas-snippet-dirs cb:yasnippet-dir)
    (yas-global-mode t)
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

;;;; Dired

(use-package dired
  :defer t
  :idle  (require 'dired)
  :init
  (hook-fn 'dired-mode-hook
    (evil-local-set-key 'normal (kbd "SPC") 'dired-hide-subdir)
    (evil-local-set-key 'normal (kbd "S-SPC") 'dired-hide-all)
    (local-set-key (kbd "M-N") 'dired-next-subdir)
    (local-set-key (kbd "M-P") 'dired-prev-subdir)
    (set (make-local-variable 'auto-revert-interval) 0.1)
    (set (make-local-variable 'auto-revert-verbose) nil)
    (auto-revert-mode +1))
  :config
  (progn
    (after 'hl-line

      (defun cb:line-is-dired-header? ()
        (equal 'dired-header
               (ignore-errors
                 (save-excursion
                   (move-to-column 3)
                   (face-at-point)))))

      (defadvice global-hl-line-highlight (around suppress-on-subdir-header activate)
        "Do not highlight the line if looking at a dired header."
        (if (and (derived-mode-p 'dired-mode) (cb:line-is-dired-header?))
            (global-hl-line-unhighlight)
          ad-do-it))

      (defadvice hl-line-highlight (around suppress-on-subdir-header activate)
        "Do not highlight the line if looking at a dired header."
        (if (and (derived-mode-p 'dired-mode) (cb:line-is-dired-header?))
            (hl-line-unhighlight)
          ad-do-it)))

    (setq dired-auto-revert-buffer t
          dired-listing-switches "-al --group-directories-first")
    (when (equal system-type 'darwin)
      ;; Use GNU version of ls if available.
      (-when-let (gls (executable-find "gls"))
        (setq ls-lisp-use-insert-directory-program t
              insert-directory-program gls)))))

(use-package dired-aux
  :defer t
  :init
  (progn
    (after 'dired (require 'dired-aux))
    (hook-fn 'dired-mode-hook
      (evil-local-set-key 'normal (kbd "TAB") 'dired-hide-subdir)
      (evil-local-set-key 'normal [backtab] 'dired-hide-all)
      (evil-local-set-key 'normal [backspace] 'dired-kill-subdir)))
  :config
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(use-package dired-x
  :commands
  (dired-jump
   dired-jump-other-window)
  :init
  (progn
    ;; Don't bind C-x C-j to dired-jump - this interferes with bindings in
    ;; ansi-term.
    (setq dired-bind-jump nil)
    (after 'dired (require 'dired-x))
    (bind-key* "M-d" 'dired-jump)
    (bind-key* "M-D" 'dired-jump-other-window)))

(use-package dired-details
  :ensure   t
  :commands dired-details-install
  :init     (after 'dired (dired-details-install))
  :config   (setq-default dired-details-hidden-string "… "))

;;;; Compilation & Checking

(use-package compile
  :bind
  (("C-c b" . compile)
   ("C-c C-b" . recompile))
  :config
  (progn

    (defun cb:compile-autoclose (buf string)
      "Automatically close the compile window."
      (cond
       ((not (s-contains? "finished" string))
        (message "Compilation exited abnormally: %s" string))

       ((s-contains? "warning" (with-current-buffer buf
                                 (buffer-string)) 'ignore-case)
        (message "Compilation succeeded with warnings"))

       (t
        (ignore-errors
          (delete-window (get-buffer-window buf)))
        (message "Compilation succeeded"))))

    (hook-fn 'find-file-hook
      "Try to find a makefile for the current project."
      (when (projectile-project-p)
        (setq-local compilation-directory (projectile-project-root))))

    (defun cb:ansi-colourise-compilation ()
      (ansi-color-apply-on-region compilation-filter-start (point)))

    (setq
     compilation-window-height    12
     compilation-scroll-output    'first-error)
    (add-to-list 'compilation-finish-functions 'cb:compile-autoclose)
    (add-hook 'compilation-filter-hook 'cb:ansi-colourise-compilation)))

(use-package mode-compile
  :ensure t
  :bind
  (("C-c ."   . mode-compile-kill)
   ("C-c C-c" . mode-compile))
  :init
  (define-key prog-mode-map (kbd "C-c C-c") 'mode-compile)
  :config
  (progn
    (when (executable-find "clang")
      (setq
       cc-compilers-list (list "clang")
       cc-default-compiler "clang"
       cc-default-compiler-options "-fno-color-diagnostics -g"))

    (setq mode-compile-expert-p             t
          mode-compile-always-save-buffer-p t)))

(use-package flyspell
  :diminish flyspell-mode
  :defer    t
  :init
  (progn
    (setq ispell-dictionary "english")
    (add-hook 'text-mode-hook 'flyspell-mode)
    (hook-fn 'cb:xml-modes-hook
      (unless (derived-mode-p 'markdown-mode)
        (flyspell-prog-mode)))
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
   (bind-key* "C-'" 'flyspell-auto-correct-word)
   (define-key flyspell-mouse-map [down-mouse-3] 'flyspell-correct-word)
   (define-key flyspell-mouse-map [mouse-3] 'undefined)))

(use-package flyspell-lazy
  :ensure  t
  :defer   t
  :init (add-hook 'flyspell-mode-hook 'flyspell-lazy-mode))

(use-package flycheck
  :ensure t
  :commands
  (flycheck-mode
   flycheck-may-enable-mode)
  :init
  (--each '(prog-mode-hook text-mode-hook)
    (hook-fn it
      (when (flycheck-may-enable-mode)
        (flycheck-mode +1))))
  :config
  (defadvice flycheck-buffer (around dont-throw-in-ido-for-fuck-sake activate)
    (condition-case _
        ad-do-it
      (user-error))))

;;;; Tags

(use-package cb-tags
  :commands cb:build-ctags
  :bind
  (("C-]"     . cb:find-tag)
   ("C-c C-r" . cb:load-ctags))
  :config
  (progn
    ;; Ensure tags searches are case-sensitive.
    (setq tags-case-fold-search nil)
    (global-set-key (kbd "M-.") 'find-tag)))

(use-package ctags-update
  :ensure   t
  :defer    t
  :diminish ctags-auto-update-mode
  :init   (add-hook 'prog-mode-hook 'turn-on-ctags-auto-update-mode))

(use-package etags-select
  :ensure   t
  :commands (etags-select-find-tag-at-point etags-select-find-tag))

;;;; Language utils

(use-package paredit
  :ensure t
  :idle   (require 'paredit)
  :diminish paredit-mode
  :commands
  (paredit-mode
   enable-paredit-mode
   disable-paredit-mode)
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

    (defun cb:paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))

    (define-key paredit-mode-map (kbd "M-)")
      'cb:paredit-wrap-round-from-behind)

    (define-key paredit-mode-map (kbd "M-[")
      'paredit-wrap-square)

    (define-key paredit-mode-map (kbd "M-{")
      'paredit-wrap-curly)

    (define-key paredit-mode-map (kbd "M-r") nil)

    (add-hook 'cb:lisp-modes-hook 'enable-paredit-mode)))

(use-package smartparens
  :ensure t
  :idle   (require 'smartparens)
  :diminish smartparens-mode
  :commands
  (smartparens-mode
   smartparens-global-mode)
  :init
  (progn
    (add-hook 'text-mode-hook 'turn-on-smartparens-mode)
    (hook-fn 'comint-mode-hook (smartparens-mode +1))

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
  :idle   (require 'smart-operator)
  :commands
  (smart-insert-operator
   smart-insert-operator-hook)
  :init
  (progn

    (defmacro smart-op (op)
      "Make a smart operator command that will insert OP."
      `(command (smart-insert-operator ,op)))

    (defun cb:python-equals ()
      "Insert an '=' char padded by spaces, except in function arglists."
      (interactive)
      (if (string-match-p
           (rx (* space) "def ")
           (thing-at-point 'line))
          (insert-string "=")
        (smart-insert-operator "=")))

    (hook-fn 'cb:python-modes-hook
      (smart-insert-operator-hook)
      (local-set-key (kbd "=") 'cb:python-equals)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'cb:ruby-modes-hook
      (smart-insert-operator-hook)
      (local-set-key (kbd "=") 'cb:python-equals)
      (local-set-key (kbd "~") (smart-op "~"))
      (local-unset-key (kbd "%"))
      (local-unset-key (kbd "&"))
      (local-unset-key (kbd "/"))
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'cb:markup-modes-hook
      (local-set-key (kbd ",") (smart-op ",")))

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
      (local-unset-key (kbd "."))))

  :config
  (defadvice smart-insert-operator (around normal-insertion-for-string activate)
    "Do not perform smart insertion if looking at a string."
    (if (-contains? '(font-lock-string-face
                      font-lock-doc-face
                      font-lock-doc-string-face
                      font-lock-comment-face)
                    (face-at-point))
        (insert (ad-get-arg 0))
      ad-do-it)))

(use-package lambda-mode
  :defer t
  :idle  (require 'lambda-mode)
  :diminish lambda-mode
  :commands lambda-mode
  :init
  (progn
    (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
    (add-hook 'cb:scheme-modes-hook    'lambda-mode)
    (add-hook 'inferior-lisp-mode-hook 'lambda-mode)
    (add-hook 'lisp-mode-hook          'lambda-mode)
    (add-hook 'cb:elisp-modes-hook     'lambda-mode)
    (add-hook 'cb:python-modes-hook    'lambda-mode)
    (add-hook 'cb:slime-modes-hook     'lambda-mode)))

(use-package paren
  :defer  t
  :idle   (require 'paren)
  :init   (hook-fn 'prog-mode-hook (require 'paren))
  :config (show-paren-mode +1))

(use-package highlight-parentheses
  :ensure t
  :defer  t
  :commands highlight-parentheses-mode
  :diminish highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

(use-package highlight-symbol
  :ensure   t
  :diminish highlight-symbol-mode
  :commands highlight-symbol-mode
  :init     (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config   (setq highlight-symbol-idle-delay 0.5))

;;;; Markup

(defun cb:xml-one-liner? (str)
  (save-match-data
    (-when-let (match (->> str (s-match
                                (rx bol (* (any space "\t"))
                                    "<" (group (+ word)) (* nonl) ">"
                                    (group (+ nonl))
                                    "</" (group (* word)) ">"
                                    (* (any space "\t")) eol))))
      (and (equal (nth 1 match)
                  (nth 3 match))
           (not (s-contains? "<" (nth 2 match)))))))

(defun cb:pp-xml (xml)
  "Return a reformatted version of an XML string.
Puts each XML node on a separate line, except for one-liners."
  (with-temp-buffer
    (insert xml)
    (nxml-mode)
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx (not (any "%")) ">"
                (group-n 1 (* (any space "	")))
                "<" (not (any "%")))
            nil t)
      (unless (cb:xml-one-liner? (thing-at-point 'line))
        (replace-match "\n" nil nil nil 1)))
    (buffer-string)))

(defun cb:reformat-xml ()
  "Insert newlines and indent XML.
  Operates on region, or the whole buffer if no region is defined."
  (interactive)
  (let* ((line (line-number-at-pos))
         (col  (current-column))
         (reg (if (region-active-p)
                  (list (region-beginning) (region-end))
                (list (point-min) (point-max))))
         (str (apply 'buffer-substring reg)))
    (atomic-change-group
      (apply 'delete-region reg)
      (insert (cb:pp-xml str))
      (apply 'indent-region reg))
    ;; Return to original position.
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)))

(use-package nxml-mode
  :commands nxml-mode
  :modes ("\\.xml" . nxml-mode)
  :init
  (progn
    (hook-fn 'nxml-mode-hook
      (local-set-key (kbd "M-q") 'cb:reformat-xml))

    (hook-fn 'find-file-hook
      "Enable nxml-mode if when visiting a file with a DTD."
      (when (s-starts-with? "<?xml " (buffer-string))
        (nxml-mode)))))

(use-package sgml-mode
  :defer t
  :init
  (hook-fn 'sgml-mode-hook
    (setq-default sgml-xml-mode t)
    (local-set-key (kbd "M-q") 'cb:reformat-xml)))

(use-package html-mode
  :defer t
  :init
  (after 'auto-complete
    (defconst cb:html-tag-attrs
      '("class" "id")
      "Global attributes that appear in HTML tags.")

    (ac-define-source html-tag-source
      '((candidates . (-when-let (tag (te/current-tag))
                        (and (te/eligible-for-auto-attribute-insert?)
                             (not (s-starts-with? "%" (te/get tag :name)))
                             (--none? (te/has-attribute it tag)
                                      cb:html-tag-attrs)

                             cb:html-tag-attrs)))
        (symbol     . "a")
        (requires   . '(tagedit))
        (action     . (lambda ()
                        (insert "=\"\"")
                        (unless (thing-at-point-looking-at ">")
                          (just-one-space))
                        (search-backward "=")
                        (forward-char 2)))))

    (add-to-list 'ac-modes 'html-mode)
    (hook-fn 'html-mode-hook
      (auto-complete-mode +1)
      (add-to-list 'ac-sources 'ac-source-html-tag-source))))

(use-package tagedit
  :ensure   t
  :commands tagedit-mode
  :init     (add-hook 'cb:xml-modes-hook 'tagedit-mode))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"          . markdown-mode)
         ("\\.[mM]arkdown$" . markdown-mode))
  :config
  (hook-fn 'markdown-mode-hook
    (buffer-face-set `(:family ,(serif-font) :height 130))
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

;;;; Lisps

(use-package parenface-plus
  :ensure t
  :defer  t
  :idle   (require 'parenface-plus)
  :init
  (hook-fn 'prog-mode-hook (require 'parenface-plus)))

(use-package eval-sexp-fu
  :commands eval-sexp-fu-flash-mode
  :init     (add-hook 'cb:lisp-modes-hook 'eval-sexp-fu-flash-mode)
  :config   (setq eval-sexp-fu-flash-duration 0.2))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (add-hook 'cb:lisp-modes-hook 'turn-on-eldoc-mode))

(use-package slime
  :defer t
  :commands
  (slime-mode
   slime)
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

;;;; Elisp

(use-package lisp-mode
  :defer t
  :mode  (("Carton" . emacs-lisp-mode))
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

    (define-key emacs-lisp-mode-map (kbd "C-c C-t") 'ert)
    (define-key emacs-lisp-mode-map (kbd "C-c e b") 'eval-buffer)
    (define-key emacs-lisp-mode-map (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'cb:switch-to-ielm)
    (define-key emacs-lisp-mode-map (kbd "C-c e r") 'eval-region)

    (hook-fn 'ielm-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-elisp))

    (defun cb:special-elisp-file? ()
      (and (derived-mode-p 'emacs-lisp-mode)
           (-contains? '("*scratch*" ".dir-locals.el")
                       (buffer-name))))

    (defun cb:elisp-after-save ()
      "Check parens are balanced and byte-compile."
      (check-parens)
      (ignore-errors
        (unless (or (cb:special-elisp-file?) no-byte-compile)
          (byte-compile-file (buffer-file-name)))))

    (hook-fn 'emacs-lisp-mode-hook
      (add-hook 'after-save-hook 'cb:elisp-after-save t 'local))

    (hook-fn 'flycheck-mode-hook
      "Disable flycheck mode for scratch buffer."
      (when (cb:special-elisp-file?)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (flycheck-mode -1)) nil t)))

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
                            "after"
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
             (group-n 1
                      (or (group (* (not space))
                                 (or "cl-" "--" "/" ":") "def" "declare")
                          "declare"
                          "define")
                      (+ (not space)))
             (+ space)
             (group-n 2 (+ (regex "\[^ )\n\]"))))
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

(use-package edebug
  :defer t
  :init
  (progn
    (autoload 'edebug-next-mode "edebug")
    (hook-fn 'emacs-lisp-mode-hook
      (local-set-key (kbd "C-x X d") 'edebug-defun))))

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
      (evil-local-set-key 'normal (kbd "M-.")
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

;;;; Clojure

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
      (-when-let (buf (last-buffer-for-mode 'clojure-mode))
        (pop-to-buffer buf)))

    (defun cb:eval-last-clj-buffer ()
      "Evaluate that last active clojure buffer without leaving the repl."
      (interactive)
      (-when-let (buf (last-buffer-for-mode 'clojure-mode))
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
  :commands
  (ac-nrepl-setup
   ac-nrepl-doc)
  :init
  (progn
    (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
    (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
    (add-to-list 'ac-modes 'nrepl-mode))
  :config
  (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(use-package cb-overtone
  :commands
  (maybe-enable-overtone-mode
   cb:stop-overtone)
  :init     (add-hook 'clojure-mode-hook 'maybe-enable-overtone-mode))

(use-package midje-mode
  :ensure   t
  :commands midje-mode
  :diminish midje-mode
  :init    (add-hook 'clojure-mode-hook 'midje-mode))

;;;; Scheme

(use-package geiser
  :ensure t
  :commands run-geiser
  :init
  (after "auto-complete"
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

;;;; Python

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
      (-when-let (buf (last-buffer-for-mode 'python-mode))
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

;;;; Ruby

(define-derived-mode erb-mode html-mode
  "ERB" nil
  (when (fboundp 'flycheck-mode)
    (flycheck-mode -1)))

(add-to-list 'auto-mode-alist `("\\.html\\.erb" . erb-mode))
(after 'auto-complete
  (add-to-list 'ac-modes 'erb-mode))

(use-package ruby-mode
  :ensure t
  :mode
  (("\\.rake\\'". ruby-mode)
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
  :init
  (add-to-list 'completion-ignored-extensions ".rbc")
  :config
  (progn
    (defun cb:rockets->hash-syntax ()
      "Convert old-style rockets to new hash literal syntax in the current buffer."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp (rx ":" (group-n 1 (+ (not space)))
                                          (* space)
                                          "=>"
                                          (* space))
                                      nil t)
          (replace-match "\\1: " t nil))))

    (defun cb:inside-ruby-class-def? ()
      (save-excursion
        (let ((end (save-excursion (search-backward-regexp (rx bol "end") nil t))))
          (search-backward-regexp (rx bol "class") end t))))

    (ac-define-source ruby-class-keywords
      '((available  . cb:inside-ruby-class-def?)
        (candidates . '("public" "private" "protected"))
        (symbol     . "s")))

    (ac-define-source ruby-class-attributes
      '((available  . cb:inside-ruby-class-def?)
        (candidates . '("attr_accessor" "attr_reader" "attr_writer"))
        (action     . (lambda () (insert " :")))
        (symbol     . "m")))

    (add-to-list 'ac-modes 'ruby-mode)
    (add-to-list 'ac-modes 'inf-ruby-mode)

    (hook-fn 'cb:ruby-modes-hook
      (add-to-list 'ac-sources 'ac-source-yasnippet)
      (add-to-list 'ac-sources 'ac-source-ruby-class-keywords)
      (add-to-list 'ac-sources 'ac-source-ruby-class-attributes)
      (subword-mode +1))))

(use-package rubocop
  :ensure t
  :commands (rubocop-run-on-project
             rubocop-run-on-directory
             rubocop-run-on-current-file))

(use-package robe
  :ensure t
  :diminish robe-mode
  :defer t
  :commands robe-mode
  :init (add-hook 'cb:rails-modes-hook 'robe-mode))

(use-package rinari
  :ensure t
  :diminish rinari-minor-mode
  :commands rinari-minor-mode
  :init
  (progn
    (add-hook 'cb:rails-modes-hook 'rinari-minor-mode)

    (hook-fn 'rinari-minor-mode-hook
      ;; Rebind rinari keys.
      (define-prefix-command 'cb:rinari-map)
      (local-set-key (kbd "C-c f") 'cb:rinari-map)
      (define-key cb:rinari-map ";" 'rinari-find-by-context)
      (define-key cb:rinari-map "C" 'rinari-find-cells)
      (define-key cb:rinari-map "F" 'rinari-find-features)
      (define-key cb:rinari-map "M" 'rinari-find-mailer)
      (define-key cb:rinari-map "S" 'rinari-find-steps)
      (define-key cb:rinari-map "Y" 'rinari-find-sass)
      (define-key cb:rinari-map "a" 'rinari-find-application)
      (define-key cb:rinari-map "c" 'rinari-find-controller)
      (define-key cb:rinari-map "e" 'rinari-find-environment)
      (define-key cb:rinari-map "f" 'rinari-find-file-in-project)
      (define-key cb:rinari-map "h" 'rinari-find-helper)
      (define-key cb:rinari-map "i" 'rinari-find-migration)
      (define-key cb:rinari-map "j" 'rinari-find-javascript)
      (define-key cb:rinari-map "l" 'rinari-find-lib)
      (define-key cb:rinari-map "m" 'rinari-find-model)
      (define-key cb:rinari-map "n" 'rinari-find-configuration)
      (define-key cb:rinari-map "o" 'rinari-find-log)
      (define-key cb:rinari-map "p" 'rinari-find-public)
      (define-key cb:rinari-map "r" 'rinari-find-rspec)
      (define-key cb:rinari-map "s" 'rinari-find-script)
      (define-key cb:rinari-map "t" 'rinari-find-test)
      (define-key cb:rinari-map "u" 'rinari-find-plugin)
      (define-key cb:rinari-map "v" 'rinari-find-view)
      (define-key cb:rinari-map "w" 'rinari-find-worker)
      (define-key cb:rinari-map "x" 'rinari-find-fixture)
      (define-key cb:rinari-map "y" 'rinari-find-stylesheet)
      (define-key cb:rinari-map "z" 'rinari-find-rspec-fixture)
      ;; Configure mode.
      (setq
       rinari-tags-file-name    "TAGS"
       rng-nxml-auto-validate-flag nil
       nxml-degraded            t))))

(use-package rsense
  :ensure t
  :defer  t
  :init
  (hook-fn 'cb:ruby-modes-hook
    (require 'rsense)
    (add-to-list 'ac-sources 'ac-source-rsense-method)
    (add-to-list 'ac-sources 'ac-source-rsense-constant))
  :config
  (progn
    (define-path cb:rsense-home "bin/rsense-0.3")
    (setq rsense-home cb:rsense-home)))

(use-package yari
  :ensure t
  :commands yari
  :init
  (hook-fn 'cb:ruby-modes-hook
    (local-set-key (kbd "C-c C-h") 'yari)))

(use-package inf-ruby
  :ensure   t
  :commands
  (inf-ruby-mode
   ruby-send-region)
  :init
  (after 'ruby-mode
    (defun cb:switch-to-ruby ()
      "Toggle between irb and the last ruby buffer.
Start an inferior ruby if necessary."
      (interactive)
      (if (derived-mode-p 'inf-ruby-mode)
          (switch-to-buffer-other-window
           (last-buffer-for-mode 'ruby-mode))
        (run-ruby)))

    (defun cb:ruby-eval-dwim ()
      "Perform a context-sensitive evaluation."
      (interactive)
      ;; Start ruby if neccessary.
      (unless (get-buffer "*ruby*")
        (run-ruby)
        (cb:switch-to-ruby)
        ;; Revert window layout.
        (when (= 2 (length (window-list)))
          (delete-other-windows)))
      (cond
       ;; Evaluate active region.
       ((region-active-p)
        (ruby-send-region (region-beginning) (region-end)))
       ;; Evaluate the block at or just before point.
       ((or (thing-at-point-looking-at
             (rx (or "end" "]" "}" ")") (* space) (* "\n")))
            (ruby-block-contains-point (point)))
        (ruby-send-block))
       ;; Eval the block-like thing around point.
       (t
        (ruby-send-region (line-beginning-position)
                          (line-end-position)))))

    (defun cb:format-irb-error (lines)
      "Return a propertized error string for the given LINES of
an irb error message."
      (-when-let* ((err (--first (s-matches? "Error:" it) lines))
                   (colon (s-index-of ":" err)))
        (concat (propertize (substring err 0 colon) 'face 'error)
                (substring err colon))))

    (defun cb:apply-ruby-font-lock (str)
      "Apply ruby font-locking to string STR."
      (with-temp-buffer
        (insert str)
        (require 'ruby-mode)
        ;; Configure ruby font-lock.
        (set (make-local-variable 'font-lock-defaults)
             '((ruby-font-lock-keywords) nil nil))
        (set (make-local-variable 'syntax-propertize-function)
             'ruby-syntax-propertize-function)

        (font-lock-fontify-buffer)
        (buffer-string)))

    (defun cb:filter-irb-output (str &rest _)
      "Print IRB output to messages."
      (ignore-errors
        (when (and (fboundp 'inf-ruby-proc) (inf-ruby-proc))
          (let ((lines
                 (->> (s-lines str)
                   (--remove (or (s-contains? "--inf-ruby" it)
                                 (s-blank? it)
                                 (s-matches? inf-ruby-prompt-pattern it)))
                   (-map 's-trim))))
            (message (or (cb:format-irb-error lines)
                         (cb:apply-ruby-font-lock (car (reverse lines))))))))
      str)

    (hook-fn 'ruby-mode-hook
      (local-set-key (kbd "C-c C-c") 'cb:ruby-eval-dwim)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-ruby))

    (hook-fn 'inf-ruby-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-ruby)
      (add-hook 'comint-preoutput-filter-functions 'cb:filter-irb-output)
      ;; Stop IRB from echoing input.
      (setq comint-process-echoes t)
      (inf-ruby-setup-keybindings))))

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
  :mode
  (("\\.yaml$" . yaml-mode)
   ("\\.yml$"  . yaml-mode)))

(use-package rvm
  :ensure t
  :commands
  (rvm-use-default
   rvm-activate-corresponding-ruby
   rvm-use
   rvm-open-gem)
  :init
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby))

;;;; Haskell

(after 'flycheck

  (defconst cb:haskell-checker-regexes
    `((,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]*\\):[ \t\n\r]*"
                "\\(?4:Warning: \\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)") warning)

      (,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]*\\):[ \t\n\r]*"
                "\\(?4:\\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)") error)))

  ;; (flycheck-declare-checker haskell-hdevtools
  ;;   "Haskell checker using hdevtools"
  ;;   :command '("hdevtools"
  ;;              "check"
  ;;              "-g" "-Wall"
  ;;              "-g" "-i/../src"
  ;;              source-inplace)
  ;;   :error-patterns cb:haskell-checker-regexes
  ;;   :modes 'haskell-mode
  ;;   :next-checkers '(haskell-hlint))

  (flycheck-declare-checker haskell-ghc
    "Haskell checker using ghc"
    :command '("ghc" "-v0"
               "-Wall"
               "-i./../src"
               "-fno-code"
               source-inplace)
    :error-patterns cb:haskell-checker-regexes
    :modes 'haskell-mode
    :next-checkers '(haskell-hlint))

  (flycheck-declare-checker haskell-hlint
    "Haskell checker using hlint"
    :command '("hlint" source-inplace)
    :error-patterns
    `((,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): Error: "
                "\\(?4:\\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)")
       error)
      (,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): Warning: "
                "\\(?4:\\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)")
       warning))
    :modes 'haskell-mode)

  (add-to-list 'flycheck-checkers 'haskell-ghc)
  ;; (add-to-list 'flycheck-checkers 'haskell-hdevtools)
  (add-to-list 'flycheck-checkers 'haskell-hlint))

(use-package haskell-mode
  :ensure t
  :commands
  (haskell-mode
   haskell-c-mode
   haskell-cabal-mode
   hoogle)
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
      (-when-let (buf (last-buffer-for-mode 'haskell-mode))
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
  :defer t
  :init
  (hook-fn 'haskell-mode-hook
    (require 'cb-haskell)
    (flycheck-mode +1)
    (flycheck-select-checker 'haskell-ghc)
    (local-set-key (kbd "C-c j") 'haskell-test<->code)))

(use-package ghc
  :ensure   t
  :commands ghc-init
  :init     (add-hook 'haskell-mode-hook 'ghc-init)
  :config
  (progn

    ;; HACK: this command throws errors on haskell-mode startup.
    (defadvice ghc-uniq-lol (around ignore-errs activate)
      (ignore-errors ad-do-it))

    (defun ac-haskell-candidates ()
      "Auto-complete source using ghc-doc."
      (let ((pat (buffer-substring (ghc-completion-start-point) (point))))
        (all-completions pat (ghc-select-completion-symbol))))

    (ac-define-source ghc
      '((candidates . ac-haskell-candidates)))

    (hook-fn 'haskell-mode-hook
      (add-to-list 'ac-sources 'ac-source-ghc))))

(use-package haskell-ac
  :defer t
  :init
  (hook-fn 'cb:haskell-modes-hook
    (require 'auto-complete)
    (add-to-list 'ac-modes major-mode)
    (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
    (add-to-list 'ac-sources 'ac-source-haskell)))

(use-package haskell-edit
  :commands (haskell-find-type-signature
             haskell-reformat-type-signature))

(use-package haskell-indentation
  :diminish haskell-indentation-mode
  :defer    t
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

;;;; Clang

(after 'cc-mode
  (define-key c-mode-map (kbd "C-c C-c") 'mode-compile)
  (define-key c-mode-map (kbd "M-q") 'indent-dwim)
  (require 'smart-operator)
  (require 'flycheck)
  (require 'smartparens))

(defun looking-at-c-flow-control-header? ()
  (thing-at-point-looking-at
   (rx (* nonl) (32 ";") (* space)
       (or "if" "when" "while" "for")
       (* nonl)
       "("
       (* (not (any ")"))))))

(after 'smart-operator

  (defun cb:c-insert-smart-op (str)
    "Insert a smart operator with special formatting in certain expressions."
    (if (looking-at-c-flow-control-header?)
        (insert str)
      (smart-insert-operator str)))

  (defun c-insert-smart-minus ()
    "Insert a minus with padding unless a unary minus is more appropriate."
    (interactive)
    (atomic-change-group
      (if (thing-at-point-looking-at
           (rx (or "(" "[" "(" ";" "=") (* space)))
          (insert "-")
        (unless (->> (buffer-substring (line-beginning-position) (point))
                  (s-trim-left)
                  (s-blank?))
          (just-one-space))
        (insert "-")
        (just-one-space))))

  (defun c-insert-smart-star ()
    "Insert a * with padding in multiplication contexts."
    (interactive)
    (if (thing-at-point-looking-at (rx (not digit) (* space)))
        (insert "*")
      (unless (->> (buffer-substring (line-beginning-position) (point))
                (s-trim-left)
                (s-blank?))
        (just-one-space))
      (insert "*")
      (just-one-space)))

  (hook-fn 'c-mode-hook
    (macrolet ((c-smart-op (char) `(command (cb:c-insert-smart-op ,char))))
      (local-set-key (kbd ",") (command (insert ",") (just-one-space)))
      (local-set-key (kbd "%") (smart-op "%"))
      (local-set-key (kbd "=") (c-smart-op "="))
      (local-set-key (kbd "+") (c-smart-op "+"))
      (local-set-key (kbd "|") (smart-op "|"))
      (local-set-key (kbd "-") 'c-insert-smart-minus)
      (local-set-key (kbd "*") 'c-insert-smart-star))))

(after 'flycheck

  (defun clang-parse-line (line)
    (ignore-errors
      (destructuring-bind (_ file line col level message)
          (s-match
           (rx (group-n 1 (+ nonl)) ":"  ; file
               (group-n 2 (+ digit)) ":" ; line
               (group-n 3 (+ digit)) ":" ; col
               (* space)
               (group-n 4 (+ nonl)) ":" ; level
               (* space)
               (group-n 5 (+ nonl))     ; message
               ) line)
        (flycheck-error-new
         :line (string-to-int line)
         :column (string-to-int col)
         :level (if (equal "error" level) 'error 'warning)
         :message message
         :filename file))))

  (defun clang-error-parser (str &rest _)
    (->> (s-lines str) (-map 'clang-parse-line) (-remove 'null)))

  (flycheck-declare-checker clang
    "Compiles the current file with clang. Used if there is no makefile."
    :command
    '("clang" "-O0" "-Wall"
      "-fsyntax-only" "-fno-color-diagnostics" "-fno-caret-diagnostics"
      "-fno-diagnostics-show-option"
      source-inplace)
    :error-parser 'clang-error-parser
    :predicate '(derived-mode-p 'c-mode))

  (add-to-list 'flycheck-checkers 'clang))

(after 'smartparens
  (defadvice sp--self-insert-command
    (before flow-control-keyword-insert-space activate)
    "Insert a space after a flow control keyword in c modes."
    (when  (and (derived-mode-p 'c-mode 'cc-mode 'c++-mode)
                (thing-at-point-looking-at
                 (rx (or "if" "when" "while" "for"))))
      (just-one-space))))

(use-package cedit
  :commands
  (cedit-forward-char
   cedit-backward-char
   cedit-end-of-statement
   cedit-beginning-of-statement
   cedit-down-block
   cedit-up-block-backward
   cedit-up-block-forward
   cedit-slurp
   cedit-wrap-brace
   cedit-barf
   cedit-splice-killing-backward
   cedit-raise
   cedit-or-paredit-slurp
   cedit-or-paredit-barf
   cedit-or-paredit-splice-killing-backward
   cedit-or-paredit-raise))

(use-package google-c-style
  :ensure   t
  :defer    t
  :commands google-set-c-style
  :init    (add-hook 'c-mode-common-hook 'google-set-c-style))

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
    (define-path cb:clang-complete-dir "lib/clang-complete-async/")
    (setq ac-clang-complete-executable (concat cb:clang-complete-dir "clang-complete"))))

(use-package c-eldoc
  :ensure   t
  :commands c-turn-on-eldoc-mode
  :init     (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;;;; SuperCollider

(use-package sclang
  :commands
  (sclang-mode
   sclang-start)
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
    (hook-fn 'sclang-mode-hook
      (smartparens-mode +1))))

(use-package sclang-snippets
  :ensure t
  :defer t
  :init
  (hook-fn 'sclang-mode
    (sclang-snippets-initialize)
    (add-to-list 'ac-sources 'ac-source-yasnippet)))

(use-package sclang-extensions
  :ensure   t
  :commands sclang-extensions-mode
  :init     (add-hook 'sclang-mode-hook 'sclang-extensions-mode))

;;;; Misc language modes

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
  ((".gitignore$" . conf-mode)
   (".gitmodules$" . conf-mode)))

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

;;;; Git

(define-prefix-command 'git-map)
(bind-key "C-x g" 'git-map)

(use-package magit
  :ensure t
  :defer  t
  :idle   (require 'magit)
  :commands magit-status
  :bind
  (("C-x g t" . magit-stash)
   ("C-x g c" . magit-checkout)
   ("C-x g u" . magit-pull)
   ("C-x g r" . magit-reflog)
   ("C-x g l" . magit-log)
   ("C-x g s" . magit-show)
   ("C-x g x" . magit-reset-head)
   ("C-x g X" . magit-reset-head-hard)
   ("C-x g d" . magit-diff-working-tree)
   ("C-x g D" . magit-diff))
  :init
  (declare-modal-executor magit-status
    :command magit-status
    :bind    "M-G")
  :config
  (progn
    (declare-ido-wrapper magit-read-top-dir)
    (declare-modal-view magit-status)
    (declare-modal-view magit-log)
    (declare-modal-view magit-reflog)
    (declare-modal-view magit-diff-working-tree)
    (declare-modal-view magit-diff)

    (defadvice magit-show (after delete-window-on-kill activate)
      "When the buffer is killed, delete its corresponding window."
      (add-hook 'kill-buffer-hook 'delete-window nil t))

    (define-combined-hook cb:magit-command-hook
      ;; Search through interned symbols for magit hooks.
      (let (hooks)
        (mapatoms (lambda (sym)
                    (let ((str (symbol-name sym)))
                      (when (and (s-starts-with? "magit-" str)
                                 (s-ends-with? "-command-hook" str))
                        (setq hooks (cons sym hooks))))))
        hooks))

    (hook-fn 'cb:magit-command-hook
      "Update modelines to ensure vc status is up-to-date."
      (force-mode-line-update t))

    (add-hook 'magit-log-edit-mode-hook 'cb:append-buffer)
    (add-hook 'magit-mode-hook 'magit-load-config-extensions)))

(use-package magit-blame
  :commands magit-blame-mode
  :bind ("C-x g b" . magit-blame-mode))

(use-package git-auto-commit-mode
  :ensure t
  :commands git-auto-commit-mode
  :init
  (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t)))

(use-package git-gutter
  :ensure t
  :bind ("C-x g g" . git-gutter:toggle)
  :commands
  (git-gutter:toggle
   git-gutter:clean
   git-gutter))

(use-package gist
  :ensure t
  :commands
  (gist-list
   gist-region
   gist-region-private
   gist-buffergist-buffer-private
   gist-region-or-buffer
   gist-region-or-buffer-private))

(use-package gitconfig-mode
  :ensure t
  :defer  t
  :modes
  (("/\\.gitconfig\\'"  . gitconfig-mode)
   ("/\\.git/config\\'" . gitconfig-mode)))

(use-package ediff
  :commands
  (ediff
   ediff-merge-files-with-ancestor)
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

;;;; Org

(use-package org
  :ensure t
  :defer  t
  :idle   (require 'org)
  :init
  (progn

    (hook-fn 'cb:org-minor-modes-hook
      "Diminish org minor modes."
      (--each cb:org-minor-modes
        (ignore-errors (diminish it))))

    (setq
     org-directory (concat user-home-directory "org/")
     org-default-notes-file (concat org-directory "notes.org"))

    (declare-modal-executor org-notes
      :command (find-file org-default-notes-file)
      :bind    "M-O")

    (when (or (daemonp) (display-graphic-p))
      (setq initial-buffer-choice org-default-notes-file))

    (macrolet ((maybe (f) `(lambda ()
                             (unless (derived-mode-p
                                      'org-mode
                                      'sgml-mode
                                      'magit-log-edit-mode)
                               (funcall ,f)))))
      ;; Use org commands in other modes.
      (add-hook 'message-mode-hook 'turn-on-orgstruct++)
      (add-hook 'message-mode-hook 'turn-on-orgtbl)
      (add-hook 'text-mode-hook (maybe 'turn-on-orgstruct++))
      (add-hook 'text-mode-hook (maybe 'turn-on-orgtbl))))
  :config
  (progn

    (defmacro with-previous-buffer (&rest forms)
      `(with-current-buffer (nth 1 (buffer-list))
         ,@body))

    (defmacro prev-str-val (sym)
      `(or (ignore-errors
             (with-previous-buffer
              ,sym))
           ""))

    (setq
     org-catch-invisible-edits 'smart
     org-pretty-entities       t

     org-capture-templates
     '(("t" "Todo" entry
        (file+headline org-default-notes-file "Tasks")
        "* TODO %?\n %i\n")

       ("r" "Reading List" entry
        (file+headline org-default-notes-file "Reading List")
        "* %?\n %i\n")

       ("l" "Link" entry
        (file+headline org-default-notes-file "Links")
        "* %(prev-str-val w3m-buffer-title)%?\n %(prev-str-val w3m-current-url)\n%i\n")

       ("n" "Note" item
        (file+headline org-default-notes-file "Notes")
        "- %?\n %i\n"))

     org-todo-keywords
     '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

    (--each '("NOTES" "COMMENTS")
      (add-to-list 'org-drawers it))

    (hook-fn 'org-mode-hook
      (auto-revert-mode +1)
      (unless (buffer-file-name)
        (cb:append-buffer))

      ;; HACK: Something in org's setup interferes with input method. Wait
      ;; til after buffer is fully initialized before setting.
      (run-with-timer 0.02 nil 'set-input-method "TeX"))

    (define-key org-mode-map (kbd "M-p") 'org-metaup)
    (define-key org-mode-map (kbd "M-n") 'org-metadown)))

(use-package org-capture
  :bind ("M-o" . org-capture)
  :init
  (hook-fn 'org-capture-mode-hook
    (when (fboundp 'evil-insert)
      (evil-insert 0)))
  :config
  (progn

    (defun cb:sort-tasks-in-subtree ()
      "Sort child elements of the tree at point."
      (let ((beg (1+ (line-number-at-pos)))
            (end (save-excursion
                   (org-mark-subtree)
                   (region-end))))
        (push-mark beg)
        (push-mark end)
        (org-sort-entries t 112)))

    (defun cb:sort-todos-by-priority ()
      "Sort the Tasks list in the notes file."
      (ignore-errors
        (with-current-buffer (find-file-noselect org-default-notes-file)
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp (rx bol "*" (+ space) "Tasks" (* space) eol) nil t)
            (cb:sort-tasks-in-subtree)))))

    (add-hook 'org-capture-after-finalize-hook 'cb:sort-todos-by-priority)))

;;;; Productivity

(use-package key-chord
  :ensure t
  :defer  t
  :init
  (progn

    (defun cb:backward-slurp ()
      (interactive)
      (cond ((cb:truthy? 'paredit-mode)
             (paredit-backward-slurp-sexp))))

    (defun cb:forward-slurp ()
      (interactive)
      (cond ((cb:truthy? 'tagedit-mode)
             (tagedit-forward-slurp-tag))
            ((cb:truthy? 'paredit-mode)
             (paredit-forward-slurp-sexp))
            (t
             (cedit-or-paredit-slurp))))

    (defun cb:splice-killing-backward ()
      (interactive)
      (cond ((cb:truthy? 'tagedit-mode)
             (tagedit-splice-tag))
            ((cb:truthy? 'paredit-mode)
             (paredit-splice-sexp-killing-backward))
            (t
             (cedit-or-paredit-splice-killing-backward))))

    (defun cb:backward-barf ()
      (interactive)
      (cond ((cb:truthy? 'paredit-mode)
             (paredit-backward-barf-sexp))
            ((cb:truthy? 'tagedit-mode)
             (tagedit-backward-barf-tag))))

    (defun cb:forward-barf ()
      (interactive)
      (cond ((cb:truthy? 'tagedit-mode)
             (tagedit-forward-barf-tag))
            ((cb:truthy? 'paredit-mode)
             (paredit-forward-barf-sexp))
            (t
             (cedit-or-paredit-barf))))

    (key-chord-define-global "x;" 'kill-current-buffer)
    (key-chord-define-global "qj" 'cb:backward-slurp)
    (key-chord-define-global "qk" 'cb:forward-slurp)
    (key-chord-define-global "ql" 'cb:splice-killing-backward)
    (key-chord-define-global "qn" 'cb:backward-barf)
    (key-chord-define-global "qm" 'cb:forward-barf)
    (key-chord-mode +1)))

(use-package scratch
  :ensure   t
  :commands scratch
  :bind     ("C-c e s" . scratch) )

(use-package iedit
  :ensure   t
  :bind
  ("C-<return>" . iedit-mode)
  :commands
  (iedit-mode
   iedit-replace-occurrences
   iedit-done)
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

    (bind-key "M-r" 'cb:rename-symbol-in-defun)
    (bind-key "M-R" 'cb:rename-symbol-in-buffer)))

(use-package info-lookmore
  :commands info-lookmore-elisp-cl
  :init     (eval-after-load "info-look" '(info-lookmore-elisp-cl)))

(use-package proced
  :defer t
  :bind ("C-x p" . proced))

(use-package ack-and-a-half
  :ensure t
  :commands
  (ack-and-a-half-same
   ack-and-a-half-find-file
   ack-and-a-half-find-file-same))

(use-package smooth-scrolling
  :ensure t)

(use-package midnight
  :ensure t
  :defer  t
  :idle (require 'midnight))

;;;; Misc commands

(defun cb:swap-with-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(bind-key* "C-;" 'cb:swap-with-previous-buffer)

;;;; Byte compilation

(defun cb:byte-compile-conf ()
  "Recompile all configuration files."
  (interactive)
  (byte-recompile-file (concat user-emacs-directory "init.el") t 0)
  (byte-recompile-directory cb:lib-dir 0 t)
  (byte-recompile-directory cb:lisp-dir 0 t))

;;;; Fortune

(with-elapsed-timer "Configuring fortune"

(defun fortune ()
  "Display a quotation from the 'fortune' program."
  (interactive)
  (-when-let (fortune (--first (ignore-errors (file-exists-p it))
                               (list (executable-find "fortune")
                                     "/usr/bin/fortune"
                                     "/usr/local/bin/fortune" )))
    (message (s-trim (shell-command-to-string (concat fortune " -s -n 250"))))))

(defun cb:show-fortune ()
  "Show fortune if started without a file to visit."
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (when (-contains? '("*scratch*"
                         "notes.org"
                         "todo.org")
                  (buffer-name))
       (fortune)))))

(hook-fn 'after-make-frame-functions (cb:show-fortune))

(add-hook 'after-init-hook 'cb:show-fortune)

)

(hook-fn 'after-init-hook
  (setq
   default-directory user-home-directory
   use-package-verbose nil)

  (load (concat user-emacs-directory "site-file.el") t t))

;; Local Variables:
;; byte-compile-warnings: (not free-vars obsolete)
;; End:

;;; init.el ends here
