;;; cb-foundation.el
;;;
;;; Basic configuration required for a sane editing environment.

(cb:require-package 'dash)
(cb:require-package 's)

(cb:require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(cb:require-package 'popwin)
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

(require 'saveplace)
(setq save-place-file (concat user-tmp-dir "places"))
(setq-default save-place t)

(require 'paren)
(show-paren-mode 1)

(require 'recentf)
(recentf-mode +1)
(setq recentf-save-file (concat user-tmp-dir "recentf")
      recentf-max-saved-items 200
      recentf-exclude '(".newsrc"
                        "ede-projects.el"
                        ".ido.last"
                        ".emacs.d/session."
                        "Map_Sym.txt"))

(require 'savehist)
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (concat user-tmp-dir "savehist"))
(savehist-mode t)

(cb:require-package 'undo-tree)
(global-undo-tree-mode)

(cb:require-package 'window-number)
(window-number-meta-mode t)

;;; Disable vc modes.
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;;; Use unique buffer names based on file path.
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;; General interaction.
(winner-mode +1)
(auto-compression-mode +1)
(setq redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      initial-major-mode 'emacs-lisp-mode
      initial-scratch-message nil
      x-select-enable-clipboard t)
(set-default 'indent-tabs-mode nil)

;;; Encodings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; File-handling
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Buffers

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Suppress \"Active processes exist\" query when exiting Emacs."
  (cl-flet ((process-list ()))
    ad-do-it))

(defvar cb:kill-buffer-ignored-list '("*scratch*" "*Messages*" "*GROUP*"))

(defun cb:kill-current-buffer ()
  "Kill the current buffer.
If this buffer is a member of `kill-buffer-ignored-list, bury it rather than killing it."
  (interactive)
  (if (member (buffer-name (current-buffer)) cb:kill-buffer-ignored-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-x C-K") 'cb:kill-current-buffer)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'cb-foundation)
