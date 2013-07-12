;;; cb-foundation.el --- Base configuration

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0033

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

;; Base configuration

;;; Code:


(require 'use-package)
(require 'cb-lib)
(require 'noflet)

(define-path cb:lib-dir       "lib/" t)
(define-path cb:lisp-dir      "lisp/" t)
(define-path cb:src-dir       "src")
(define-path cb:tmp-dir       "tmp/")
(define-path cb:elpa-dir      "elpa/")
(define-path cb:bin-dir       "bin/")
(define-path cb:etc-dir       "etc/")
(define-path cb:yasnippet-dir "snippets/")
(define-path cb:backups-dir   "backups/")
(define-path cb:autosaves-dir "tmp/autosaves/")

(defalias 'yes-or-no-p 'y-or-n-p)
(autoload 'ido-yes-or-no-p "ido-yes-or-no")
(autoload 'edebug-step-mode "edebug")
(autoload 'server-running-p "server")

(hook-fn 'after-init-hook
  (unless (server-running-p)
    (server-start)))

(use-package simple
  :diminish
  (visual-line-mode
   global-visual-line-mode
   auto-fill-mode))

;; Use the version of emacs in /src for info and source.
(setq source-directory (format "%s/emacs-%s.%s" cb:src-dir
                               emacs-major-version
                               emacs-minor-version))
(setenv "INFOPATH" (concat source-directory "/info/"))


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
 fill-column                  80)

(add-hook 'text-mode-hook 'visual-line-mode)
(icomplete-mode +1)
(global-set-key (kbd "RET") 'comment-indent-new-line)

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

(put 'activate-input-method 'safe-local-eval-function t)
(put 'set-input-method 'safe-local-eval-function t)

;;; Exiting Emacs

;; Rebind to C-c k k ("kill") to prevent accidentally exiting when
;; using Org bindings.
(bind-key* "C-x C-c" (command (message "Type <C-c k e> to exit Emacs")))
(bind-key* "C-c k k" 'cb:exit-emacs-dwim)
(bind-key* "C-c k e" 'cb:exit-emacs)

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
         (widen)
         (recenter))

        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        (t
         (narrow-to-defun))))

(after 'evil
  (bind-key "M-n" 'cb:narrow-dwim))
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
     ((f-dir? file)
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
  (noflet ((process-list () nil))
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

(defadvice comment-indent-new-line (after add-space activate)
  "Add a space after opening a new comment line."
  (unless (thing-at-point-looking-at (rx (+ space)))
    (just-one-space)))

;;; Basic hooks

(defun cb:next-dwim ()
  "Perform a context-sensitive 'next' action."
  (interactive)
  (cond
   ((truthy? 'edebug-active)
    (edebug-step-mode))
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

(hook-fn 'after-init-hook
  "Ensure the user-home-directory is used as the default path,
rather than the app bundle."
  (setq default-directory user-home-directory)

  (load (concat user-emacs-directory "site-file.el") t t))

;;; View behaviour

(declare-modal-view package-list-packages)

;; Disable backups for files edited with tramp.
(after 'backup-dir
  (add-to-list 'bkup-backup-directory-info
               (list tramp-file-name-regexp ""))
  (setq tramp-bkup-backup-directory-info nil))

;;; Comint

;; Make comint read-only. This will stop the prompts from being editable
;; in inferior language modes.
(setq comint-prompt-read-only t)

(defun cb:append-buffer ()
  "Enter insertion mode at the end of the current buffer."
  (interactive)
  (goto-char (point-max))
  (if (fboundp 'evil-append-line)
      (evil-append-line 1)
    (end-of-line)))

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

(provide 'cb-foundation)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-foundation.el ends here
