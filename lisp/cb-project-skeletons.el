;;; cb-project-skeletons.el --- Create project skeletons.

;; Copyright (C) 2013 Chris Barrett

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

;; Creates project skeletons. Most of this code is taken from @magnar's init.el:
;; <https://github.com/magnars/.emacs.d/blob/master/site-lisp/project-skeletons/project-skeletons.el>.

;;; Code:

(require 'dash)
(require 's)
(require 'cb-lib)
(require 'cb-paths)

(defvar skel-project-skeletons nil
  "The list of available project skeletons.")

(defvar skel-folder (f-join user-emacs-directory "project-skeletons")
  "The directory containing project skeletons.")

(defvar skel-project-folder (f-join user-home-directory "Projects")
  "The directory where projects should be created.")

(defun skel:join-patterns (patterns)
  "Turn PATTERNS into a string that `find' can use."
  (mapconcat (lambda (pat) (format "-name \"%s\"" pat))
             patterns " -or "))

(defun skel:files-matching (patterns folder &optional type)
  "Find candidate files for performing replacements.

* PATTERNS is sequence of strings to search for.

* FOLDER is the path to a directory to search.

* TYPE is an argument to the `find' command."
  (split-string (shell-command-to-string
                 (format "find %s %s \\( %s \\) | head -n %s"
                         folder
                         (or type "")
                         (skel:join-patterns patterns)
                         1000))))

(defun skel:instantiate-template-file (file replacements)
  "Initialise an individual file.

* FILE is the path to the file.

* REPLACEMENTS is an alist of substitutions to perform in the file."
  (with-temp-file file
    (insert-file-contents-literally file)
    (--each replacements
      (goto-char 0)
      (while (search-forward (car it) nil t)
        (replace-match (cdr it))))))

(defun skel-instantiate-template-directory (template folder &rest replacements)
  "Create the directory for TEMPLATE at destination FOLDER.
Performs the substitutions specified by REPLACEMENTS."
  (declare (indent 2))
  (let ((tmp-folder (expand-file-name (concat "__skeleton_tmp_" template) skel-project-folder)))
    (when (f-exists? tmp-folder) (f-delete tmp-folder t))
    (copy-directory (expand-file-name template skel-folder) tmp-folder nil nil t)
    (--each (skel:files-matching (--map (s-concat "*" (car it) "*") replacements) tmp-folder)
      (rename-file it (s-replace-all replacements it)))
    (--each (skel:files-matching ["*"] tmp-folder "-type f")
      (skel:instantiate-template-file it replacements))
    (copy-directory tmp-folder folder)
    (delete-directory tmp-folder t)))

(defmacro with-new-project (project-name skeleton replacements &rest body)
  "Declare a new project.

* PROJECT-NAME is a string naming the project.

* SKELETON is a string describing the project type.

* REPLACEMENTS is an alist of (string . replacement) used specify
  substitutions when initialising the project from its skeleton.

* BODY forms are executed at the project root once the project is created."
  (declare (indent 2))
  `(let ((folder (expand-file-name ,project-name skel-project-folder)))

     (message "Initialising project...")

     (skel-instantiate-template-directory ,skeleton folder ,@replacements)
     (view-buffer-other-window skel-out)
     (let ((default-directory (concat folder "/")))
       ,@body
       (async-shell-command
        "git init && git add -A && git commit -m 'Initial commit'"
        (format "*init skeleton [%s]*" folder)))

     (message "Initialising project...Done")))

(defun declare-project-skeleton (name fn)
  "Add project skeleton to the list of available ones.

* NAME is the name of the project.

* FN is an interactive command that will be called to initialise
  the project."
  (add-to-list 'skel-project-skeletons (cons name fn)))

(defun create-project (type)
  "Create a project of the given TYPE."
  (interactive
   (list (completing-read "Skeleton: " (-map 'car skel-project-skeletons) nil t)))
  (call-interactively (cdr (assoc type skel-project-skeletons))))

;; Load skeletons.
(when (file-exists-p skel-folder)
  (--each (directory-files skel-folder nil "^[^#].*el$")
    (load (expand-file-name it skel-folder))))


(provide 'cb-project-skeletons)

;;; cb-project-skeletons.el ends here
