;;; cb-lisp.el --- Configuration for all lisp modes

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0005

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

;; Configuration for all lisp modes

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-mode-groups)

;; Set keys for all lisp modes.
(hook-fn 'cb:lisp-modes-hook
  (local-set-key (kbd "M-q") 'indent-dwim))

;; Configure special paren formatting behaviours for lisp modes.
(after 'smartparens

  ;; Add lisp modes to `sp-navigate-reindent-after-up'.
  ;; Provides Paredit-style paren reindentation when closing parens.
  (let ((ls (assoc 'interactive sp-navigate-reindent-after-up)))
    (setcdr ls (-uniq (-concat (cdr ls) cb:lisp-modes))))

  (defun cblisp:just-inserted-double-quotes? (id action ctx)
    (and (sp-in-string-p id action ctx)
         (s-matches? (rx (not (any "\\")) "\"" eol)
                     (buffer-substring (line-beginning-position) (point)))))

  (defun sp-lisp-just-one-space (id action ctx)
    "Pad LISP delimiters with spaces."
    (when (and (equal 'insert action)
               (or (sp-in-code-p id action ctx)
                   (cblisp:just-inserted-double-quotes? id action ctx)))
      ;; Insert a leading space, unless
      ;; 1. this is a quoted form
      ;; 2. this is the first position of another list
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
    ;; Pad delimiters with spaces.
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-lisp-just-one-space))
    (sp-local-pair "{" "}" :post-handlers '(:add sp-lisp-just-one-space))
    (sp-local-pair "[" "]" :post-handlers '(:add sp-lisp-just-one-space))
    (sp-local-pair "(" ")" :post-handlers '(:add sp-lisp-just-one-space))
    (sp-local-pair "'" nil :actions nil))

  ;; Reserve backtick pair handling in Elisp for hyperlinks.
  (sp-local-pair (-difference cb:lisp-modes cb:elisp-modes)
                 "`" "`" :when '(sp-in-string-p)))

;; `parenface-plus' adds a face for parentheses allowing, parens to be dimmed.
(use-package parenface-plus
  :ensure t
  :config
  ;; Ensure paren face is displayed the first time a candidate buffer is loaded.
  (--each cb:lisp-modes
    (font-lock-add-keywords it '(("(\\|)" . paren-face)))))

;; `eval-sexp-fu' adds a flash when running eval-expression.
(use-package eval-sexp-fu
  :commands turn-on-eval-sexp-fu-flash-mode
  :init (add-hook 'cb:lisp-modes-hook 'turn-on-eval-sexp-fu-flash-mode)
  :config (setq eval-sexp-fu-flash-duration 0.2))

;; `eldoc' shows documentation hints in the minibuffer.
(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (add-hook 'cb:lisp-modes-hook 'turn-on-eldoc-mode))

;; `slime' adds support for inferior Common Lisp processes.
(use-package slime
  :ensure   t
  :defer    t
  :commands (slime-setup slime)
  :init
  (defun run-slime ()
    "Run slime, prompting for a lisp implementation."
    (interactive)
    (let ((current-prefix-arg '-))
      (slime)))
  :config
  (setq slime-lisp-implementations `((lisp ("sbcl" "--noinform")))))

;; `ac-slime' adds an auto-complete source for slime.
(use-package ac-slime
  :ensure   t
  :defer    t
  :commands (set-up-slime-ac)
  :init     (add-hook 'slime-modes-hook 'set-up-slime-ac)
  :config   (add-to-list 'ac-modes 'slime-repl-mode))

(provide 'cb-lisp)

;; Local Variables:
;; End:

;;; cb-lisp.el ends here
