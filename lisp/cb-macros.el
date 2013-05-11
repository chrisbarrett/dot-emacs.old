;;; cb-macros --- Common macros used in my emacs config.

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

;; Common macros used in my emacs config.

;;; Code:

(require 'dash)
(require 'cl-lib)

(defmacro hook-fn (hook &optional docstring &rest body)
  "Execute forms when a given hook is called.
The arguments passed to the hook function are bound to the symbol 'args'.

* HOOK is the name of the hook.

* DOCSTRING optionally documents the forms.  Otherwise, it is
  evaluated as part of BODY.

* BODY is a list of forms to evaluate when the hook is run."
  (declare (indent 1) (doc-string 2))
  `(add-hook ,hook (lambda (&rest args)
                     ,@(cons docstring body))))

(defmacro progn-after-load (feature &rest body)
  "Execute BODY forms after FEATURE is loaded."
  (declare (indent 1))
  `(eval-after-load ,feature '(progn ,@body)))

;;; ----------------------------------------------------------------------------

(defun directory-p (f)
  "Test whether F is a directory.  Return nil for '.' and '..'."
  (and (file-directory-p f)
       (not (string-match "/[.]+$" f))))

(defun directory-subfolders (path)
  "Return a flat list of all subfolders of PATH."
  (->> (directory-files path)
    (--map (concat path it))
    (-filter 'directory-p)))

(defun cb:prepare-load-dir (dir add-path)
  "Create directory DIR if it does not exist.
If ADD-PATH is non-nil, add DIR and its children to the load-path."
  (let ((dir (concat user-emacs-directory dir)))
    (unless (file-exists-p dir) (make-directory dir))
    (when add-path
      (--each (cons dir (directory-subfolders dir))
        (add-to-list 'load-path it)))
    dir))

(defmacro cb:define-path (sym path &optional add-path)
  "Define a subfolder of the `user-emacs-directory'.
This directory tree will be added to the load path if ADD-PATH is non-nil."
  `(defconst ,sym (cb:prepare-load-dir ,path ,add-path)))

(provide 'cb-macros)

;;; cb-macros.el ends here
