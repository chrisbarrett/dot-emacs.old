;;; config-git.el --- Configuration for git utilities and magit

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

;; Configuration for git utilities and magit

;;; Code:

(require 'utils-common)
(require 'utils-ui)
(require 'config-ido)
(require 'config-evil)

(cb:install-package 'magit t)

(declare-modal-executor magit-status
  :command (call-interactively 'magit-status)
  :bind    "M-G")

(declare-ido-wrapper magit-read-top-dir)
(declare-modal-view magit-status)
(declare-modal-view magit-log)
(declare-modal-view magit-reflog)
(declare-modal-view magit-diff-working-tree)
(declare-modal-view magit-diff)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(after '(magit evil)

  (evil-global-set-keys 'normal
    "g P" 'magit-key-mode-popup-pushing
    "g c" 'magit-key-mode-popup-committing
    "g l" 'magit-log
    "g r" 'magit-reflog
    "g D" 'magit-diff-working-tree
    "g B" 'magit-blame-mode
    "g b" (command
           (with-window-restore
             (magit-branch-manager)
             (buffer-local-set-key (kbd "q") (command (restore))))))

  (define-keys magit-diff-mode-map
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up
    "j"   'evil-next-line
    "k"   'evil-previous-line
    "/"   'evil-search-forward)

  (hook-fn 'git-rebase-mode-hook
    (when (true? evil-mode)
      (evil-emacs-state)))

  )

(after 'dired
  (define-key dired-mode-map (kbd "M-G") 'magit-status))

(define-combined-hook cb:magit-command-hook
  (--filter-atoms (s-matches? "^magit-.*-command-hook$" (symbol-name it))))

(hook-fn 'cb:magit-command-hook
  (force-mode-line-update t))

(diminish 'magit-auto-revert-mode)

(cb:install-package 'git-auto-commit-mode)

(add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))

(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(put 'git-commit-mode 'fill-column 72)

(add-hook 'git-commit-mode-hook 'cb:maybe-evil-insert-state)

(cb:install-package 'gist)

(cb:install-package 'gitconfig-mode)

(cb:install-package 'git-gutter+)
(unless noninteractive
  (global-git-gutter+-mode +1))

(hook-fn 'after-save-hook
  (when (true? git-gutter+-mode)
    (ignore-errors
      (git-gutter+-refresh))))

(defvar cb-git:gutter-refresh-idle-timer
  (unless noninteractive
    (run-with-idle-timer 1.5 t (lambda ()
                                 (when (true? git-gutter+-mode)
                                   (ignore-errors (git-gutter+-refresh)))))))

(hook-fn 'git-gutter+-mode-hook
  (diminish 'git-gutter+-mode))

(defvar magit-read-top-dir 'magit-read-top-dir)

(after 'git-gutter+
  (defun git-gutter+-close-commit-edit-buffer ()
    "Abort edits and discard commit message being composed."
    (interactive)
    (remove-hook 'kill-buffer-hook 'server-kill-buffer t)
    (remove-hook 'kill-buffer-query-functions 'git-commit-kill-buffer-noop t)
    (let ((clients (git-commit-buffer-clients)))
      (if clients
          (dolist (client clients)
            (server-send-string client "-error Commit aborted by user")
            (delete-process client))
        (kill-buffer)))

    (set-window-configuration git-gutter+-pre-commit-window-config)))

(defun cb-git:add ()
  "Run 'git add' on the file for the current buffer."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (user-error "Buffer has no corresponding file"))
   ((not (vc-git-root (buffer-file-name)))
    (user-error "Not a git repository"))
   ((yes-or-no-p "Stage all changes to this file?")
    (save-buffer)
    (vc-git-register (list (buffer-file-name)))
    (message "Done.")))
  (git-gutter+-refresh))

(after '(git-gutter+ evil)
  (evil-global-set-keys 'normal
    "g n" (lambda (arg)
            (interactive "p")
            (git-gutter+-refresh)
            (git-gutter+-next-hunk arg))
    "g p" (lambda (arg)
            (interactive "p")
            (git-gutter+-refresh)
            (git-gutter+-previous-hunk arg))
    "g h" 'git-gutter+-popup-hunk
    "g x" 'git-gutter+-revert-hunk
    "g s" 'git-gutter+-stage-hunks
    "g a" 'cb-git:add)

  (add-hook 'magit-commit-mode-hook 'cb:maybe-evil-insert-state))

(cb:install-package 'git-gutter-fringe+)
(after 'git-gutter+ (require 'git-gutter-fringe+))

(provide 'config-git)

;;; config-git.el ends here
