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
  (evil-global-set-key 'normal "(" 'sp-backward-up-sexp)
  (evil-global-set-key 'normal ")" 'sp-forward-sexp)

  ;; Define a special state for smartparens operations.

  (evil-define-state paren "Paren editing state."
    :tag "<Paren> "
    :message "-- PAREN --"
    :cursor (bar . 2))

  (hook-fn 'evil-paren-state-entry-hook
    (when (equal last-command 'evil-end-of-line)
      (forward-char)))

  ;; Configure entry and exit from paren state.
  (evil-global-set-key 'normal (kbd ",") 'evil-paren-state)
  (define-key evil-paren-state-map (kbd "ESC") 'evil-normal-state)
  (define-key evil-paren-state-map (kbd "C-g") 'evil-normal-state)
  ;; Define paren state keys.
  (evil-global-set-keys 'paren
    "A" 'sp-add-to-previous-sexp
    "a" 'sp-add-to-next-sexp
    "B" 'sp-backward-barf-sexp
    "b" 'sp-forward-barf-sexp
    "c" 'sp-convolute-sexp
    "D" 'sp-backward-kill-sexp
    "d" 'sp-kill-sexp
    "e" 'sp-emit-sexp
    "j" 'sp-join-sexp
    "K" 'sp-splice-sexp-killing-backward
    "k" 'sp-splice-sexp-killing-forward
    "n" 'sp-next-sexp
    "p" 'sp-previous-sexp
    "r" 'sp-raise-sexp
    "s" 'sp-splice-sexp-killing-around
    "t" 'sp-transpose-sexp
    "U" 'sp-backward-unwrap-sexp
    "u" 'sp-unwrap-sexp
    "w" 'sp-rewrap-sexp
    "x" 'sp-split-sexp
    "Y" 'sp-backward-copy-sexp
    "y" 'sp-copy-sexp
    "<" 'sp-beginning-of-sexp
    "," 'sp-end-of-sexp))

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
      (global-set-key (kbd it) (eval `(command (sp-insert-or-up ,it _arg)))))


    ;; Bind Paredit-style wrapping commands.
    (sp-pair "(" ")" :bind "M-(")
    (sp-pair "{" "}" :bind "M-{")
    (sp-pair "[" "]" :bind "M-[")
    (sp-pair "\"" "\"" :bind "M-\"")
    (sp-pair "`" "`" :bind "M-~")
    (sp-pair "'" "'"
             :bind "M-'"
             :when '(:add sp-in-code-p)
             :unless '(:add sp-in-string-p))

    ;; Do not use apostrophe pair in text modes.
    (sp-with-modes '(text-mode org-mode markdown-mode magit-log-edit-mode)
      (sp-local-pair "'" "'" :actions '(:rem insert)))

    (defun cb-sp:kill-blank-lines (&optional arg)
      (interactive "P")
      (cond
       ((emr-blank-line?)
        (kill-whole-line))
       (t
        (sp-kill-sexp nil arg))))

    (define-keys sp-keymap
      "C-<backspace>" 'sp-backward-up-sexp
      "DEL"           'sp-backward-delete-char
      "C-k"           'cb-sp:kill-blank-lines)

    ;; Use bind-key for keys that tend to be overridden.
    (bind-key "C-M-," 'sp-backward-down-sexp sp-keymap)
    (bind-key "C-M-." 'sp-next-sexp sp-keymap)))

(provide 'cb-smartparens)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-smartparens.el ends here
