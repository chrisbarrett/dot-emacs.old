;;; super-smart-ops.el --- Like smart operator, but better

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

;; Like smart operator, but better

;;; Code:

(require 'utils-common)
(require 'utils-buffers)
(require 'config-yasnippet)
(require 'config-evil)

(defvar-local smart-op-list
  '("=" "<" ">" "%" "+" "-" "*" "/" "&" "|" "!" ":")
  "A list of strings to treat as operators.")

(defun smart-op-in-string-or-comment? ()
  "Non-nil if point is in a string or comment."
  (nth 8 (syntax-ppss)))

(defun cb-op:prev-non-space-char ()
  "Return the previous non-whitespace character on this line, as a string."
  (save-excursion
    (when (search-backward-regexp (rx (not space))
                                  (line-beginning-position) t)
      (thing-at-point 'char))))

(defun cb-op:delete-horizontal-space-non-readonly ()
  "Delete horizontal space around point that is not read-only."
  (while (and (not (eobp))
              (s-matches? (rx space) (char-to-string (char-after)))
              (not (get-char-property (point) 'read-only)))
    (forward-char 1))

  (while (and (not (bobp))
              (s-matches? (rx space) (char-to-string (char-before)))
              (not (get-char-property (1- (point)) 'read-only)))
    (delete-char -1)))

(defun cb-op:maybe-just-one-space-after-operator ()
  "Insert a trailing space unless:
- the next char is an operator
- we are in a parenthesised operator."
  (unless (or (and (not (eolp))
                   (-contains? smart-op-list (char-to-string (char-after))))
              (thing-at-point-looking-at
               (eval `(rx "(" (+ (or ,@smart-op-list)) ")"))))
    (just-one-space)))

(defun smart-insert-op (op)
  "Insert a smart operator OP, unless we're in a string or comment."

  ;; Narrow to the current active snippet field if yasnippet is active. This
  ;; prevents errors when attempting to delete whitespace outside the current
  ;; field.
  (yas-with-field-restriction

    (cond
     ((or (smart-op-in-string-or-comment?)
          ;; Looking at quotation mark?
          (-contains? '(?\" ?\') (char-after)))
      (insert op))

     ((-contains? (cl-list* "(" smart-op-list) (cb-op:prev-non-space-char))
      (cb-op:delete-horizontal-space-non-readonly)
      (insert op)
      (cb-op:maybe-just-one-space-after-operator))

     (t
      (unless (s-matches? (rx bol (* space) eol)
                          (buffer-substring (line-beginning-position) (point)))
        (just-one-space))

      (insert op)
      (cb-op:maybe-just-one-space-after-operator)))))

(defmacro make-smart-op (str)
  "Return a function that will insert smart operator STR.
Useful for setting up keymaps manually."
  (let ((fname (intern (concat "smart-insert-op/" str))))
    `(progn
       (defun ,fname ()
         "Auto-generated command.  Inserts a smart operator."
         (interactive "*")
         (smart-insert-op ,str))
       ',fname)))

(defun cb-op:add-smart-ops (ops custom)
  (let ((custom-ops (-map 'car custom)))
    (setq-local smart-op-list (-union ops custom-ops))
    (--each ops
      (local-set-key (kbd it) (eval `(make-smart-op ,it))))
    (--each custom
      (cl-destructuring-bind (op . fn) it
        (local-set-key (kbd op) fn)))))

(cl-defun declare-smart-ops (mode &key add rem custom)
  "Define the smart operators for the given mode.

- MODE is the mode to add the smart ops for.

- ADD is a list of smart operators to add to the defaults.

- REM is a list of smart operators to remove from the defaults.

- CUSTOM is a list of special operator insertion commands to use
  instead of the defaults. It is an alist of (OP . FUNCTION),
  where OP is a string and FUNCTION is a symbol."
  (declare (indent 1))
  (cl-assert (symbolp mode))
  (cl-assert (null (-intersection add rem)))
  (cl-assert (null (-intersection add (-map 'car custom))))
  (cl-assert (null (-intersection rem (-map 'car custom))))

  (let ((hook (intern (concat (symbol-name mode) "-hook")))
        (ops (-union (-map 'car custom)
                     (-difference (-union smart-op-list add) rem))))

    ;; Set smart ops list for buffers that already exist.
    (--each (--filter-buffers (derived-mode-p mode))
      (with-current-buffer it
        (cb-op:add-smart-ops ops custom)))
    ;; Set smart ops in mode's hook.
    (add-hook hook `(lambda ()
                      (cb-op:add-smart-ops ',ops ',custom)))

    (list :mode mode :ops ops)))

(defun cb-op:delete-last-smart-op ()
  "Delete the last smart-operator that was inserted."
  (unless (or (derived-mode-p 'text-mode) (smart-op-in-string-or-comment?))
    (save-restriction
      (narrow-to-region (line-beginning-position) (point))

      (when (s-matches? (concat (regexp-opt smart-op-list) " *$")
                        (buffer-substring (line-beginning-position) (point)))
        ;; Delete op
        (let ((op-pos
               (save-excursion
                 (search-backward-regexp (regexp-opt smart-op-list)))))
          (while (and (/= (point) op-pos)
                      (not (get-char-property (point) 'read-only)))
            (delete-char -1)))

        ;; Delete preceding spaces.
        (cb-op:delete-horizontal-space-non-readonly)
        t))))

(defadvice sp-backward-delete-char (around delete-smart-op activate)
  "Delete the smart operator that was just inserted, including padding."
  (or (cb-op:delete-last-smart-op) ad-do-it))

(defadvice smart-insert-op (around restrict-to-insert-state activate)
  "If evil mode is active, only insert in insert state."
  (cond
   ((and (true? evil-mode) (evil-insert-state-p))
    ad-do-it)
   ((true? evil-mode))
   (t
    ad-do-it)))


(provide 'super-smart-ops)

;;; super-smart-ops.el ends here
