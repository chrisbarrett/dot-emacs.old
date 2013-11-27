;;; cb-idris.el --- Configuration for the Idris language.

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

;; Configuration for the Idris language.

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-mode-groups)

;; Configure smart operators for Idris
;;
;; These are more or less duplicates of the Haskell functions.

(defun cbidris:smart-comma ()
  (interactive)
  (cond
   ((s-matches? (rx bol (* space) eol)
                (buffer-substring (line-beginning-position) (point)))
    (insert ", ")
    (idris-indentation-indent-line))
   (t
    (insert ","))))

(defun cbidris:smart-pipe ()
  "Insert a pipe operator. Add padding, unless we're inside a list."
  (interactive)
  (if (s-matches? (rx "[" (* (any "|" alnum)) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (insert "|")
    (smart-insert-operator "|")))

(defun cbidris:looking-at-module-or-constructor? ()
  (-when-let (sym (thing-at-point 'symbol))
    (s-uppercase? (substring sym 0 1))))

(defun cbidris:smart-dot (&optional arg)
  "Insert a period. Add padding, unless this line is an import statement.
With a prefix arg, insert a period without padding."
  (interactive "*P")
  (cond
   (arg
    (insert "."))
   ((cbidris:looking-at-module-or-constructor?)
    (insert "."))
   ((thing-at-point-looking-at (rx (or "(" "{" "[") (* space)))
    (insert "."))
   (t
    (smart-insert-operator "."))))

(defun cbidris:smart-colon ()
  "Insert either a type binding colon pair or a cons colon."
  (interactive)
  (if (s-matches? (rx bol (* space) (? ",") (* space)
                      (+ (not (any space "("))) (* space) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (atomic-change-group
        (just-one-space)
        (insert "::")
        (just-one-space))
    (insert ":")))

(defun cbidris:insert-arrow (arrow)
  "If point is inside a tuple, insert an arrow inside.
Otherwise insert an arrow at the end of the line."
  (atomic-change-group
    (cl-destructuring-bind (&key beg end op &allow-other-keys)
        (sp-get-sexp t)
      ;; Check whether point is inside a tuple.
      (if (and (equal op "(")
               (> (point) beg)
               (< (point) end))
          (sp-end-of-sexp)
        (end-of-line)))
    ;; Insert arrow.
    (just-one-space)
    (insert arrow)
    (just-one-space)))

(defun cbidris:at-typedecl? ()
  (s-matches? (rx bow ":" eow) (buffer-substring (line-beginning-position) (point))))

(defun cbidris:smart-minus (&optional arg)
  "Insert an arrow if we're in a typesig, otherwise perform a normal insertion.
With a prefix arg, insert an arrow with padding at point."
  (interactive "*P")
  (cond
   (arg
    (just-one-space)
    (insert "->")
    (just-one-space))
   ((cbidris:at-typedecl?)
    (cbidris:insert-arrow "->"))
   (t
    (smart-insert-operator "-"))))

(defun cbidris:smart-lt (&optional arg)
  "Insert a less than symbol. With a prefix arg, insert an arrow at point."
  (interactive "*P")
  (cond
   (arg
    (just-one-space)
    (insert "<-")
    (just-one-space))
   (t
    (smart-insert-operator "<"))))

(bind-keys
  :hook cb:idris-modes-hook
  "," 'cbidris:smart-comma
  "-" 'cbidris:smart-minus
  "=" (command (smart-insert-operator "="))
  "<" 'cbidris:smart-lt
  "." 'cbidris:smart-dot
  ":" 'cbidris:smart-colon
  "|" 'cbidris:smart-pipe
  "?" (command (smart-insert-operator "?"))
  "$" (command (smart-insert-operator "$")))

(add-hook 'cb:idris-modes-hook 'smart-insert-operator-hook)

;; `idris-mode' provides editing support for the Idris language.
(use-package idris-mode
  :mode (("\\.idr$" . idris-mode))
  :config
  (after 'idris-mode
    (define-key idris-mode-map (kbd "C-c C-z") 'idris-switch-to-output-buffer)))

;; Define a command to switch from the repl to the last Idris src buffer.

(defun idris-switch-to-src ()
  "Pop to the last idris source buffer."
  (interactive)
  (-if-let (buf (car (--filter-buffers (derived-mode-p 'idris-mode))))
    (pop-to-buffer buf)
    (error "No idris buffers")))

(after 'idris-repl
  (define-key idris-repl-mode-map (kbd "C-c C-z") 'idris-switch-to-src))

(provide 'cb-idris)

;;; cb-idris.el ends here
