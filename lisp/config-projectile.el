;;; config-projectile.el --- Configuration for projectile

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

;; Configuration for projectile

;;; Code:

(require 'utils-common)

(defvar projectile-known-projects-file
  (f-join cb:tmp-dir "projectile-bookmarks.eld"))

(defvar projectile-cache-file (f-join cb:tmp-dir "projectile.cache"))

(cb:install-package 'projectile t)
(cb:install-package 'ack-and-a-half t)
(projectile-global-mode +1)

(diminish 'projectile-mode)

(autoload 'projectile-project-root "projectile")
(autoload 'projectile-project-p "projectile")

(defun cb-projectile:set-compilation-dir ()
  (when (projectile-project-p)
    (setq-local compilation-directory (projectile-project-root))))

(add-hook 'find-file-hook 'cb-projectile:set-compilation-dir)

(defun cb-projectile:eshell-project ()
  "Open an eshell buffer in the current project."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (eshell-buffer-name (projectile-project-name)))
    (cb:term-cycle)))

(setq projectile-switch-project-action
      (lambda () (call-interactively 'magit-status)))

(defun projectile-delete-project (dir)
  "Delete the given project and remove it from the index."
  (interactive (list (projectile-completing-read "Remove from known projects: "
                                                 projectile-known-projects)))
  (cond ((y-or-n-p "Project will be moved to trash.  Continue?")
         (ignore-errors (move-file-to-trash dir))
         (projectile-remove-known-project dir)
         (message "Project deleted and removed from cache."))
        (t
         (user-error "Aborted"))))

;;; Key bindings

;; Disable default keys so the discover.el interface will be displayed instead.
(define-key projectile-mode-map projectile-keymap-prefix nil)
(define-key projectile-mode-map (kbd "C-c C-SPC") 'helm-projectile)

(bind-key* "s-f" 'projectile-find-file)
(bind-key* "s-d" 'projectile-find-dir)
(bind-key* "s-l" 'projectile-switch-project)
(bind-key* "s-a" 'projectile-ack)
(bind-key* "s-t" 'cb-projectile:eshell-project)


(provide 'config-projectile)

;;; config-projectile.el ends here
