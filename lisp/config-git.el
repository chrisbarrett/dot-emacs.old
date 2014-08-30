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
(cb:install-package 'git-auto-commit-mode t)
(cb:install-package 'gist t)
(cb:install-package 'gitconfig-mode t)
(cb:install-package 'git-gutter+ t)
(cb:install-package 'git-gutter-fringe+ t)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(diminish 'magit-auto-revert-mode)
(diminish 'git-gutter+-mode)

(add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))

;;; Modal views

(declare-modal-executor magit-status
  :command (call-interactively 'magit-status)
  :bind    "M-G")

(declare-ido-wrapper magit-read-top-dir)
(declare-modal-view magit-status)
(declare-modal-view magit-log)
(declare-modal-view magit-reflog)
(declare-modal-view magit-diff-working-tree)
(declare-modal-view magit-diff)

;;; Configure git commit mode

(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(put 'git-commit-mode 'fill-column 72)

;;; Configure git gutter

(global-git-gutter+-mode +1)

(hook-fn 'after-save-hook
  (when (true? git-gutter+-mode)
    (ignore-errors
      (git-gutter+-refresh))))

(defvar cb-git:gutter-refresh-idle-timer
  (unless noninteractive
    (run-with-idle-timer 1.5 t (lambda ()
                                 (when (true? git-gutter+-mode)
                                   (ignore-errors (git-gutter+-refresh)))))))

;;; Fix errors caused by funcalling non-existent var.

(defvar magit-read-top-dir 'magit-read-top-dir)

;;; Redefine function

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

;;; Commands

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

(provide 'config-git)

;;; config-git.el ends here
