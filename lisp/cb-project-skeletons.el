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

(defvar skel-directory (f-join user-emacs-directory "project-skeletons")
  "The directory containing project skeletons.")

(defvar skel-license-directory (f-join skel-directory "licenses")
  "The directory containing license files for projects.")

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

(defun skel:instantiate-template-directory (template folder replacements)
  "Create the directory for TEMPLATE at destination FOLDER.
Performs the substitutions specified by REPLACEMENTS."
  (let ((tmp-folder (f-expand (concat "__skeleton_tmp_" template) skel-project-folder)))
    (when (f-exists? tmp-folder) (f-delete tmp-folder t))
    (copy-directory (f-expand template skel-directory) tmp-folder nil nil t)
    (--each (skel:files-matching (--map (s-concat "*" (car it) "*") replacements) tmp-folder)
      (rename-file it (s-replace-all replacements it)))
    (--each (skel:files-matching ["*"] tmp-folder "-type f")
      (skel:instantiate-template-file it replacements))
    (copy-directory tmp-folder folder)
    (delete-directory tmp-folder t)))

(defun skel:instantiate-license-file (license-file destination replacements)
  "Populate the given license file template."
  (let ((repls (cl-list* (cons "__year__" (format-time-string "%Y"))
                         (cons "__user-name__" user-full-name)
                         (cons "__organisation__" (or (true? user-organisation) user-full-name))
                         replacements)))

    (f-write (s-replace-all repls (f-read license-file))
             'utf-8
             destination)))

(defmacro with-new-project (project-name skeleton license replacements &rest body)
  "Declare a new project.

* PROJECT-NAME is a string naming the project.

* SKELETON is a string describing the project type.

* LICENSE is the path to the license file to use as a template.

* REPLACEMENTS is an alist of (string . replacement) used specify
  substitutions when initialising the project from its skeleton.

* BODY forms are executed at the project root once the project is created."
  (declare (indent 2))
  `(let ((folder (f-expand ,project-name skel-project-folder)))

     (message "Initialising project...")

     (skel:instantiate-template-directory ,skeleton folder (list ,@replacements))
     (skel:instantiate-license-file ,license (f-join folder "LICENSE") (list ,@replacements))
     (let ((default-directory (f-slash folder)))
       ,@body
       (%-async "git init && git add -A && git commit -m 'Initial commit'"))

     (message "Initialising project...done")))

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

;; License file creation.

(defun skel-read-license (prompt default)
  "Prompt the user to select a license.

* PROMPT is the prompt shown to the user.

* DEFAULT a regular expression used to find the default."
  (let* ((xs (--map (cons (s-upcase (f-filename it)) it)
                    (f-files skel-license-directory)))
         (d (car (-first (C (~ s-matches? default) car) xs)))
         (choice (ido-completing-read prompt (-map 'car xs) nil t d)))
    (cdr (assoc choice xs))))

;; Load skeletons.
(when (f-exists? skel-directory)
  (--each (directory-files skel-directory nil "^[^#].*el$")
    (load (f-expand it skel-directory))))

(provide 'cb-project-skeletons)

;;; cb-project-skeletons.el ends here
