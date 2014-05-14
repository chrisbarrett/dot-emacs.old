;;; config-lisp.el --- Configure lisp

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

;; Configure lisp

;;; Code:

(require 'utils-common)
(require 'smartparens)
(require 'config-modegroups)

(hook-fn 'cb:lisp-modes-hook
  (local-set-key (kbd "M-q") 'indent-dwim))

(let ((ls (assoc 'interactive sp-navigate-reindent-after-up)))
  (setcdr ls (-uniq (-concat (cdr ls) cb:lisp-modes))))

(defun cblisp:just-inserted-double-quotes? (id action ctx)
  (and (sp-in-string-p id action ctx)
       (s-matches? (rx (not (any "\\")) "\"" eol)
                   (buffer-substring (line-beginning-position) (point)))))

(defun sp-lisp-just-one-space (id action ctx)
  "Pad Lisp delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (cblisp:just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is a quoted form
    ;; 2. this is the first position of another list
    ;; 3. this form begins a new line.
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "," "`" "'" "@" "#" "~" "(" "[" "{")
                       ;; HACK: nREPL prompt
                       (and (any alnum "." "/" "-") ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (s-matches? (rx (or (any ")" "]" "}")
                                  eol))
                          (buffer-substring (point) (1+ (point))))
        (just-one-space)))))

(sp-with-modes cb:lisp-modes
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "{" "}" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "[" "]" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "(" ")" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "'" nil :actions nil))

(add-hook 'cb:lisp-modes-hook 'turn-on-eldoc-mode)

(hook-fn 'eldoc-mode-hook
  (diminish 'eldoc-mode))

(cb:install-package 'redshank)

(add-hook 'cb:lisp-modes-hook 'turn-on-redshank-mode)

(hook-fn 'redshank-mode-hook
  (diminish 'redshank-mode))


(provide 'config-lisp)

;;; config-lisp.el ends here
