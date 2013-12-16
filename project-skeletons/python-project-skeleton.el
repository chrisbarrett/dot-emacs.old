;;; python-project-skeleton.el --- Skeleton for Python projects

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

;; Skeleton for Python projects.

;;; Code:

(autoload 'projectile-project-root "projectile")

(defun cbpy:python-binaries ()
  "Find python binaries in bin directories."
  (--mapcat
   (f-files it (~ s-matches? (rx "python" (* (any digit "." "-")) eol)))
   '("/usr/bin" "/usr/local/bin")))

(defun cbpy:create-virtualenv-dirlocals ()
  "Create a .dir-locals file for virtualenv variables."
  (save-excursion
    (add-dir-local-variable nil 'virtualenv-default-directory default-directory)
    (add-dir-local-variable nil 'virtualenv-workon (f-filename default-directory))
    (save-buffer)
    (kill-buffer)))

(defun cbpy:init-virtualenv ()
  "Initialise a virtualenv dir in the current directory."
  (let ((python-bin (ido-completing-read "Python binary: " (cbpy:python-binaries)))
        (dest (f-join default-directory "env")))
    (unless (zerop (%-sh "virtualenv" "-p" (%-quote python-bin) dest))
      (error "Virtualenv failed"))))

(defun create-python-project (name description license)
  "Create a Python project skeleton."
  (interactive (list
                (read-string "Project name: ")
                (read-string "Description: ")
                (skel-read-license "License: " "bsd")))

  (with-new-project name "python-project" license
    ((cons "__project-name__" name)
     (cons "__description__" description))

    (cbpy:init-virtualenv)
    (let ((inhibit-redisplay t))
      (cbpy:create-virtualenv-dirlocals))

    (message "Ready")))

(declare-project-skeleton "python-project" 'create-python-project)

(provide 'python-project-skeleton)

;;; python-project-skeleton.el ends here
