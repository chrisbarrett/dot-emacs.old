;;; config-scala.el --- Configure Scala

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

;; Configure Scala

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)

(cb:declare-package-installer scala
  :match (rx (or ".scala" ".sbt"))
  :packages (scala-mode2))

(custom-set-variables
 '(scala-indent:align-forms t)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy scala-indent:eager-strategy))

;;; Snippet utils

(defun cbscala:find-case-class-parent ()
  (save-excursion
    (if (search-backward-regexp
         (rx (or
              (and bol (* space)
                   (or (and (? "abstract" (+ space)) "class")
                       "trait")
                   (+ space) (group-n 1 (+ alnum)))
              (and bol (* space)
                   "case" (+ space) "class" (* anything) space
                   "extends" (+ space) (group-n 1 (+ alnum)) (* space) eol)))
         nil t)
        (match-string 1)
      "")))

;;; Smart ops

(defun cbscala:equals ()
  (interactive)
  (super-smart-ops-insert "=")
  (just-one-space))

(defun cbscala:colon ()
  (interactive)
  (super-smart-ops-insert ":")
  (just-one-space))

(defmacro define-scala-variance-op-command (sym op)
  "Define command named SYM to insert a variance operator OP."
  `(defun ,sym ()
     "Insert a variance operator.
Pad in normal expressions. Do not insert padding in variance annotations."
     (interactive "*")
     (cond
      ;; No padding at the start of type parameter.
      ((thing-at-point-looking-at (rx "[" (* space)))
       (delete-horizontal-space)
       (insert ,op))
      ;; Leading padding after a comma, e.g. for a type parameter or function call.
      ((thing-at-point-looking-at (rx "," (* space)))
       (just-one-space)
       (insert ,op))
      ;; Otherwise leading and trailing padding.
      (t
       (super-smart-ops-insert ,op)))))

(define-scala-variance-op-command cbscala:plus "+")
(define-scala-variance-op-command cbscala:minus "-")

(super-smart-ops-configure-for-mode 'scala-mode
  :custom
  '(("=" . cbscala:equals)
    (":" . cbscala:colon)
    ("+" . cbscala:plus)
    ("-" . cbscala:minus)))

;;; Commands

(defun cbscala:join-line ()
  "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
  (interactive)
  (let (join-pos)
    (save-excursion
      (goto-char (line-end-position))
      (unless (eobp)
        (forward-line)
        (call-interactively 'scala-indent:join-line)
        (setq join-pos (point))))

    (when join-pos
      (goto-char join-pos))))

;;; Key bindings

(after 'scala-mode2
  (define-key scala-mode-map (kbd ".") nil))

(provide 'config-scala)

;;; config-scala.el ends here
