;;; cb-elisp --- Commands for Elisp editing

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

;; Commands for Elisp editing

;;; Code:

(require 's)
(require 'cl-lib)
(require 'cb-macros)

(defun cb:goto-first-match (regex)
  (save-match-data
    (when (string-match regex (buffer-string) 0)
      (goto-char (match-beginning 0)))))

(defun cb:insert-above (str)
  "Insert STR at the line above."
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (back-to-indentation)
  (insert str)
  (newline))

(defun cb:symbol-file-name (sym)
  (when-let (f (find-lisp-object-file-name sym (symbol-function sym)))
    (and (stringp f)
         (file-name-nondirectory (file-name-sans-extension f)))))

(defun extract-autoload (function file)
  "Create an autoload for FUNCTION.
FILE is the file that declares FUNCTION.
See `autoload' for details."
  (interactive
   (let* ((sym  (intern (or (thing-at-point 'symbol) (read-string "Function: "))))
          (file (or (cb:symbol-file-name sym)
                    (read-string "File: "))))
     (list sym file)))
  (save-excursion
    (let ((form  (format "(autoload '%s \"%s\")" function file)))
      (if (cb:goto-first-match "^(autoload ")
          (progn (newline) (insert form))
        (beginning-of-defun)
        (cb:insert-above form)))))

(defun cb:goto-open-round ()
  (unless (thing-at-point-looking-at "(")
    (beginning-of-sexp)
    (forward-char -1)))

(defun cb:format-function-call (name arglist)
  (format "(%s%s)" name (if (s-blank? arglist) "" (concat " " arglist))))

(defun cb:format-defun (name args body)
  (with-temp-buffer
    (lisp-mode-variables)
    (insert (format "(defun %s (%s) \n%s)" name args body))
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun extract-function (name arglist)
  "Extract a function from the sexp beginning at point.
NAME is the name of the new function.
ARGLIST is its argument list."
  (interactive "sName: \nsArglist: ")
  (cl-assert (not (s-blank? name)) t "Name must not be blank")
  (let ((args (s-trim arglist))
        (name (s-trim name)))
    (save-excursion
      (cb:goto-open-round)
      (paredit-kill)
      (insert (cb:format-function-call name args))
      (beginning-of-defun)
      (cb:insert-above (cb:format-defun name args (car kill-ring)))
      ;; Revert kill-ring pointer.
      (setq kill-ring (cdr kill-ring)))))

(defun eval-and-replace ()
  "Replace the form behind point with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case _
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; ----------------------------------------------------------------------------
;;; Font lock

(font-lock-add-keywords
 'emacs-lisp-mode
 `(
   ;; -let forms.

   (,(rx "(" (group (* (not space)) "-let") symbol-end)
    (1 font-lock-keyword-face))

   ;; cl-definition forms.

   (,(rx "(" (group (or "cl-defun" "cl-defmacro" "cb:def")
                    (* (not space)))
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))

   ;; cl-struct.

   (,(rx "(" (group "cl-defstruct")
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))
    (1 font-lock-keyword-face)
    (2 font-lock-type-face))

   ;; use-package macro.

   (,(rx "(" (group "use-package")
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face))))

(provide 'cb-elisp)

;;; cb-elisp.el ends here
