;;; cb-helm.el --- Configuration for helm

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0023

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

;; Configuration for helm

;;; Code:

(require 'use-package)
(autoload 'helm-read-file-name "helm-mode")

;; `projectile' adds project-level management and editing commands.
(use-package projectile
  :ensure   t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-known-projects-file
          (f-join cb:tmp-dir "projectile-bookmarks.eld")

          projectile-cache-file
          (f-join cb:tmp-dir "projectile.cache"))

    (defadvice find-tag (before set-tags-directory activate)
      "Ensure the TAGS path is set before searching for tags."
      (setq tags-file-name (concat (projectile-project-root) "TAGS")))

    (projectile-global-mode +1)))

;; `helm' provides commands for making interactive selections.
(use-package helm
  :ensure t
  :defer  t
  :idle   (require 'helm)
  :init
  (progn
    (after 'helm
      (define-key helm-map (kbd "C-[") 'helm-keyboard-quit))

    (after 'evil
      (bind-keys
        :overriding? t
        "C-SPC"   'helm-mini
        "C-x C-b" 'helm-buffers-list
        "M-a" 'helm-apropos
        "M-b" 'helm-buffers-list
        "M-i" 'helm-imenu
        "M-f" 'helm-etags-select
        "M-m" 'helm-man-woman))))

;; `helm-adaptive'
(use-package helm-adaptative
  :defer t
  :config
  (setq helm-adaptive-history-file (f-join cb:tmp-dir "helm-adaptive-history")))

;; `helm-projectile' adds a helm command for selecting files or buffers in the
;; current project.
(use-package helm-projectile
  :ensure t
  :commands helm-projectile
  :bind (("M-j" . helm-projectile))
  :idle (require 'helm-projectile))

;; `helm-files' provides file-search functions for helm.
(use-package helm-files
  :commands (helm-find-files-1 helm-find-files)
  :defer t
  :init
  (progn

    (bind-key* "C-x SPC" 'helm-find-files)

    (defun helm-find-in-lisp-dir (arg)
      (interactive "P")
      (let ((default-directory cb:lisp-dir))
        (helm-find-files arg)))

    (when cb:use-vim-keybindings?
      (bind-key* "M-I" 'helm-find-in-lisp-dir)))

  :config
  (after 'helm-files
    (define-key helm-find-files-map
      (kbd "~")
      (command
       (if (looking-back "/")
           (helm-insert-in-minibuffer "~/" t)
         (call-interactively 'self-insert-command))))))

(provide 'cb-helm)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-helm.el ends here
