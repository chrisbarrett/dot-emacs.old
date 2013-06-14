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

(use-package projectile
  :ensure   t
  :defer    t
  :diminish projectile-mode
  :init (add-hook 'after-init-hook 'projectile-global-mode)
  :config
  (progn
    (setq projectile-known-projects-file
          (concat cb:tmp-dir "projectile-bookmarks.eld"))
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

    (after 'evil
      (bind-key* "M-a" 'helm-apropos)
      (bind-key* "M-b" 'helm-buffers-list)
      (bind-key* "C-x C-b" 'helm-buffers-list)
      (bind-key* "M-h" 'helm-mini)
      (bind-key* "M-i" 'helm-imenu)
      (bind-key* "M-f" 'helm-etags-select)
      (bind-key* "M-m" 'helm-man-woman)
      (bind-key* "M-w" 'helm-w3m-bookmarks)
      (bind-key* "M-k" 'helm-show-kill-ring))))

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

    (after 'evil
      (bind-key* "C-j" 'cb:helm-dwim))))

(provide 'cb-helm)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-helm.el ends here
