;;; cb-tags --- ctags-related commands

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

(autoload 'eproject-root "eproject")

(defvar cb:ctags-excludes
  "--exclude=db --exclude=test --exclude=.git --exclude=public")

(defun cb:load-ctags ()
  "Create a tags file at the root of the current project, then load it."
  (interactive)
  (and (cb:build-ctags) (cb:visit-project-tags)))

(defun cb:build-ctags ()
  "Create a tags file at the root of the current project."
  (interactive)
  (message "Building project tags...")
  (let* ((root (eproject-root))
         (tags (concat root "TAGS"))
         (cmd (format "%s -e -R --extra=+fq %s -f %s"
                      (executable-find "ctags")
                      cb:ctags-excludes
                      tags)))
    (if (not (equal 0 (shell-command cmd)))
        (error "Failed to create tags")
      (message "Tags written to \"%s\"" tags))))

(defun cb:visit-project-tags ()
  "Visit the tags file at the root of the current project."
  (interactive)
  (let ((tags (concat (eproject-root) "TAGS")))
    (if (not (file-exists-p tags))
        (error "Unable to load tags.  \"%s\" does not exist" tags)
      (visit-tags-table tags)
      (message (concat "Loaded " tags)))))

(defun cb:find-tag ()
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (cb:visit-project-tags)
    (cb:build-ctags))
  (etags-select-find-tag-at-point))

(provide 'cb-tags)

;;; cb-tags.el ends here
