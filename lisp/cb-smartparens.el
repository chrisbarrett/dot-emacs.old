;;; cb-smartparens.el --- Configuration for Smartparens

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130708.0124

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

;; Configuration for Smartparens

;;; Code:

(require 'use-package)
(require 'dash)
(require 'cb-lib)
(require 's)

(after 'evil
  (defmacro define-evil-sp-key (key sym)
    "Wrapper around `evil-global-set-key' that defines key under sp prefix."
    `(evil-global-set-key 'normal ,(kbd (concat ", " key)) ,sym))

  (define-evil-sp-key "A" 'sp-add-to-previous-sexp)
  (define-evil-sp-key "a" 'sp-add-to-next-sexp)
  (define-evil-sp-key "B" 'sp-backward-barf-sexp)
  (define-evil-sp-key "b" 'sp-forward-barf-sexp)
  (define-evil-sp-key "c" 'sp-convolute-sexp)
  (define-evil-sp-key "D" 'sp-backward-kill-sexp)
  (define-evil-sp-key "d" 'sp-kill-sexp)
  (define-evil-sp-key "e" 'sp-emit-sexp)
  (define-evil-sp-key "j" 'sp-join-sexp)
  (define-evil-sp-key "K" 'sp-splice-sexp-killing-backward)
  (define-evil-sp-key "k" 'sp-splice-sexp-killing-forward)
  (define-evil-sp-key "n" 'sp-next-sexp)
  (define-evil-sp-key "p" 'sp-previous-sexp)
  (define-evil-sp-key "r" 'sp-raise-sexp)
  (define-evil-sp-key "s" 'sp-splice-sexp-killing-around)
  (define-evil-sp-key "t" 'sp-transpose-sexp)
  (define-evil-sp-key "U" 'sp-backward-unwrap-sexp)
  (define-evil-sp-key "u" 'sp-unwrap-sexp)
  (define-evil-sp-key "w" 'sp-rewrap-sexp)
  (define-evil-sp-key "x" 'sp-split-sexp)
  (define-evil-sp-key "Y" 'sp-backward-copy-sexp)
  (define-evil-sp-key "y" 'sp-copy-sexp)
  (define-evil-sp-key "<" 'sp-beginning-of-sexp)
  (define-evil-sp-key "," 'sp-end-of-sexp)

  (evil-global-set-key 'normal "(" 'sp-backward-up-sexp)
  (evil-global-set-key 'normal ")" 'sp-forward-sexp))

(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-mode +1)
    (show-smartparens-global-mode +1)
    (require 'smartparens-config)

    (defun sp-generic-leading-space (&optional id action ctx)
      "Pad ID with a leading space unless point is either:
1. at the start of a braced expression
2. at indentation."
      (when (and (equal 'insert action)
                 (sp-in-code-p id action ctx))
        (save-excursion
          (search-backward id)
          (unless (s-matches?
                   (rx (or (group bol (* space))
                           (any "(" "[" "{")) eol)
                   (buffer-substring (line-beginning-position) (point)))
            (just-one-space)))))

    (defun sp-insert-or-up (delim &optional arg)
      "Insert a delimiter DELIM if inside a string, else move up."
      (interactive "sDelimiter:\nP")
      (if (or (emr-looking-at-string?)
              (emr-looking-at-comment?))
          (insert delim)
        ;; HACK: use internal calling convention for `sp-up-sexp'. This is
        ;; needed for some functionality, e.g. re-indentation, to behave
        ;; correctly.
        (sp-up-sexp arg 'interactive)))

    (setq sp-autoinsert-if-followed-by-word t)

    ;; Close paren keys move up sexp.
    (setq sp-navigate-close-if-unbalanced t)
    (--each '(")" "]" "}")
      (global-set-key (kbd it) (command (sp-insert-or-up it _arg))))

    ;; Bind Paredit-style wrapping commands.
    (sp-pair "(" ")" :bind "M-(")
    (sp-pair "{" "}" :bind "M-{")
    (sp-pair "[" "]" :bind "M-[")
    (sp-pair "'" "'" :bind "M-'")
    ;; (sp-pair "<" ">" :bind "M-<")
    (sp-pair "\"" "\"" :bind "M-\"")
    (sp-pair "`" "`" :bind "M-~")

    (define-key sp-keymap (kbd "C-<backspace>") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "DEL") 'sp-backward-delete-char)
    (define-key sp-keymap (kbd "C-k")
      ;; kill blank lines or balanced sexps.
      (lambda (&optional arg) (interactive "P")
        (cond
         ((emr-blank-line?)
          (kill-whole-line))
         (t
          (sp-kill-sexp nil arg)))) )

    ;; Use bind-key for keys that tend to be overridden.
    (bind-key "C-M-," 'sp-backward-down-sexp sp-keymap)
    (bind-key "C-M-." 'sp-next-sexp sp-keymap)))

(provide 'cb-smartparens)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-smartparens.el ends here
