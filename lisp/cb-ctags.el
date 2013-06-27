;;; cb-ctags.el --- ctags-related commands

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

;; ctags-related commands. Adapted from:
;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/

;;; Code:

(require 'use-package)
(require 's)
(autoload 'projectile-project-root "projectile")

(defvar ctags-exclude-patterns
  '("db" "test" ".git" "public" "flycheck-"))

;;;###autoload
(defun load-ctags ()
  "Create a tags file at the root of the current project, then load it."
  (interactive)
  (and (build-ctags) (visit-ctags)))

(defun cb:format-tags-excludes ()
  (let ((sep " --exclude="))
    (concat sep (s-join sep ctags-exclude-patterns))))

(defun cb:build-tags-at (tags)
  (interactive "DDirectory: ")
  (shell-command (format "%s -e -R --extra=+fq %s -f %s"
                         (executable-find "ctags")
                         (cb:format-tags-excludes)
                         tags)))

(defun cb:tags-project-root ()
  "Return the root of the current project or the current directory."
  (or (ignore-errors (projectile-project-root))
      (s-chop-prefix "Directory " (pwd))))

(defun cb:home-subfolder? (dir)
  "Return true if DIR is a subfolder of the user home folder."
  (let* ((home (expand-file-name "~"))
         (dir  (s-chop-suffix "/" (expand-file-name dir))))
    (and (not (equal home dir))
         (s-matches? home dir))))

(defun build-ctags ()
  "Create a tags file at the root of the current project."
  (interactive)
  (message "Building project tags...")
  (let* ((dir  (cb:tags-project-root))
         (tags (concat dir "TAGS")))
    (unless (or (cb:home-subfolder? dir)
                (y-or-n-p (format "Really create tags in \"%s\"? " dir)))
      (error "Tags not created"))
    (if (equal 0 (cb:build-tags-at tags))
        (message "Tags written to \"%s\"" tags)
      (error "Failed to create tags"))))

(defun visit-ctags ()
  "Visit the tags file at the root of the current project."
  (interactive)
  (let ((tags-revert-without-query t))
    (visit-tags-table (cb:tags-project-root))
    (message (concat "Loaded " tags-file-name))))

(defun find-ctag ()
  "Find the tags at point, creating a tags file if none exists."
  (interactive)
  (if (file-exists-p (concat (cb:tags-project-root) "TAGS"))
      (visit-ctags)
    (build-ctags))
  (etags-select-find-tag-at-point))

;; Ensure tags searching is case-sensitive.
(setq tags-case-fold-search nil)
(global-set-key (kbd "M-.") 'find-ctag)
(bind-key "C-]" 'find-ctag)
(bind-key "C-c C-r" 'load-ctags)

(use-package ctags-update
  :ensure   t
  :defer    t
  :diminish ctags-auto-update-mode
  :init   (add-hook 'prog-mode-hook 'turn-on-ctags-auto-update-mode))

(use-package etags-select
  :ensure   t
  :commands (etags-select-find-tag-at-point etags-select-find-tag))

(provide 'cb-ctags)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-ctags.el ends here
