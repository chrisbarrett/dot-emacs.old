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

   ;; cb:extracting-list

   (,(rx "(" (group "cb:extracting-list") (or space eol))
    (1 font-lock-keyword-face))

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
