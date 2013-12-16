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
  "The directory where projects will be created.")

(defvar skel-default-replacements
  (list (cons "__YEAR__" (format-time-string "%Y"))
        (cons "__USER__" user-full-name)
        (cons "__ORGANISATION__" (or (true? user-organisation) user-full-name)))
  "A list of replacements available for expansion in project skeletons.")

(defvar skel-init-with-git t
  "When non-nil, initialise newly created projects with a git repository.")

(defvar skel-after-project-instantiated-hook nil
  "Hook run after a project is successfully instantiated.
Each function will be passed the path of the newly instantiated
project.")

;;;;;;;;;;;;;;;;;;;;;;;; Internal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (replace-match (cdr it) 'fixcase 'literal)))))

(defun skel:instantiate-template-directory (template dest replacements)
  "Create the directory for TEMPLATE at destination DEST.
Performs the substitutions specified by REPLACEMENTS."
  (let ((tmpd (make-temp-file "project-skeleton__" t)))
    (unwind-protect

        (progn
          (--each (f-entries (f-expand template skel-directory))
            (f-copy it tmpd))

          ;; Process file name and contents according to replacement rules.
          (--each (f-entries tmpd nil t)
            (let ((updated (s-replace-all replacements it)))
              (unless (equal updated it)
                (rename-file it updated))))

          (--each (f-files tmpd nil t)
            (skel:instantiate-template-file it replacements))

          (copy-directory tmpd dest)))

    (delete-directory tmpd t)))

(defun skel:instantiate-license-file (license-file dest replacements)
  "Populate the given license file template."
  (f-write (s-replace-all replacements (f-read license-file)) 'utf-8 dest))

(defun skel-read-license (prompt default)
  "Prompt the user to select a license.

* PROMPT is the prompt shown to the user.

* DEFAULT a regular expression used to find the default."
  (let* ((xs (--map (cons (s-upcase (f-filename it)) it)
                    (f-files skel-license-directory)))
         (d (car (-first (C (~ s-matches? default) car) xs)))
         (choice (ido-completing-read prompt (-map 'car xs) nil t d)))
    (cdr (assoc choice xs))))

(defun skel:initialize-git-repo  (dir)
  "Initialise a new git repository at DIR."
  (when skel-init-with-git
    (message "Initialising git...")
    (shell-command
     (format "cd %s && git init && git add -A && git commit -m 'Initial commit'"
             (shell-quote-argument dir)))
    (message "Initialising git...done")))

;;;;;;;;;;;;;;;;;;;;;;;; User commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(cl-defmacro define-project-skeleton
    (name &key replacements after-creation default-license)
  "Declare a new project type

* NAME is a string naming the project type.

* REPLACEMENTS is an alist of (string . replacement) used specify
  substitutions when initialising the project from its skeleton.

* DEFAULT-LICENSE is a regexp matching the name of a license to
  be used as the default when reading from the user.

* AFTER-CREATION is a unary function to be run once the project
  is created. It should take a single argument--the path to the
  newly-created project."
  (declare (indent 1))

  (let ((constructor (intern (format "create-%s" name)))
        (default-license-var (intern (format "%s-default-license" name))))
    `(progn

       (defvar ,default-license-var ,default-license
         ,(concat "Auto-generated variable.\n\n"
                  "The default license type for " name " skeletons.") )

       (defun ,constructor (project-name license-file)

         ,(concat
           "Auto-generated function.\n\n"
           "Interactively creates a new " name " skeleton.\n"
           "
* PROJECT-NAME is the name of this project instance.

* LICENSE-FILE is the path to a license file to be added to the project.")

         (interactive (list (read-string "Project name: ")
                            (skel-read-license "License: " ,default-license-var)))

         (let* ((dest (f-join skel-project-folder project-name))
                (default-directory dest)
                (repls (-concat '(("__PROJECT-NAME__" . ,name))
                                skel-default-replacements
                                ,replacements)))

           (skel:instantiate-template-directory ,name dest repls)
           (skel:instantiate-license-file license-file (f-join dest "LICENSE") repls)
           (funcall ,after-creation dest)
           (skel:initialize-git-repo dest)
           (run-hook-with-args 'skel-after-project-instantiated-hook default-directory)))

       (add-to-list 'skel-project-skeletons (cons ,name ',constructor)))))

(add-hook 'skel-after-project-instantiated-hook (lambda (dir) (dired dir)))

;;;###autoload
(defun create-project (type)
  "Create a project of the given TYPE."
  (interactive (list (completing-read "Skeleton: "
                                      (-sort 'string< (-map 'car skel-project-skeletons))
                                      nil t)))
  (let ((constructor (cdr (assoc type skel-project-skeletons))))
    (call-interactively constructor)))

;;;;;;;;;;;;;;;;;;;;;;;; Define skeletons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-project-skeleton "elisp-package"
  :default-license "^gpl"
  :after-creation
  (lambda (dir)
    (%-async (format "cd %s && cask" (%-quote dir)))))

(defun cbpy:python-binaries ()
  "Find python binaries in bin directories."
  (--mapcat
   (f-files it (~ s-matches? (rx "python" (* (any digit "." "-")) eol)))
   '("/usr/bin" "/usr/local/bin")))

(defun cbpy:create-virtualenv-dirlocals (dir)
  "Create a .dir-locals file for virtualenv variables."
  (save-excursion
    (add-dir-local-variable nil 'virtualenv-default-directory dir)
    (add-dir-local-variable nil 'virtualenv-workon (f-filename dir))
    (save-buffer)
    (kill-buffer)))

(defun cbpy:init-virtualenv (dir)
  "Initialise a virtualenv environment at DIR."
  (let ((python-bin (ido-completing-read "Python binary: " (cbpy:python-binaries)))
        (dest (f-join dir "env")))
    (unless (zerop (%-sh "virtualenv" "-p" (%-quote python-bin) dest))
      (error "Virtualenv failed"))))

(define-project-skeleton "python-project"
  :default-license "^bsd"
  :after-creation
  (lambda (dir)
    (cbpy:init-virtualenv dir)
    (let ((inhibit-redisplay t))
      (cbpy:create-virtualenv-dirlocals dir))))

(provide 'cb-project-skeletons)

;;; cb-project-skeletons.el ends here
