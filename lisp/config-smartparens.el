;;; config-smartparens.el --- Configuration for smartparens

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

;; Configuration for smartparens

;;; Code:

(require 'utils-common)
(require 'config-evil)
(require 'config-modegroups)

(cb:install-package 'smartparens t)
(smartparens-global-mode)

(hook-fns '(prog-mode-hook cb:markup-modes-hook)
  (smartparens-strict-mode +1))

(show-smartparens-global-mode +1)

(require 'smartparens-config)

(diminish 'smartparens-mode)

(sp-pair "(" ")" :bind "M-(")
(sp-pair "{" "}" :bind "M-{")
(sp-pair "[" "]" :bind "M-[")
(sp-pair "\"" "\"" :bind "M-\"")
(sp-pair "`" "`" :bind "M-`")

(sp-with-modes (-flatten (list cb:ruby-modes
                               cb:python-modes
                               'shell-script-mode
                               'makefile-mode
                               'conf-mode))
  (sp-local-pair
   "'" "'"
   :bind "M-'"
   :actions '(:add insert)
   :when '(:add sp-in-code-p)
   :unless '(:add sp-in-string-p)))

(setq sp-autoinsert-if-followed-by-word t)

(sp-with-modes (-flatten
                (list cb:haskell-modes
                      cb:lisp-modes
                      cb:idris-modes
                      cb:prompt-modes
                      'org-mode
                      'tuareg-mode
                      'minibuffer-inactive-mode
                      'text-mode))
  (sp-local-pair "'" "'" :actions '(:rem insert)))

(define-keys sp-keymap
  "C-<backspace>" 'sp-backward-up-sexp
  "DEL"           'sp-backward-delete-char)

;; Use bind-key for keys that tend to be overridden.
(bind-key "C-M-," 'sp-backward-down-sexp sp-keymap)
(bind-key "C-M-." 'sp-next-sexp sp-keymap)

(defun cb-sp:kill-blank-lines ()
  (interactive)
  (cond
   ((s-blank? (s-trim (current-line)))
    (kill-whole-line))
   (t
    (call-interactively 'sp-kill-sexp)
    (when (s-blank? (s-trim (current-line)))
      (let ((pt (point)))
        (join-line)
        (goto-char (1+ pt)))))))

(define-key sp-keymap (kbd "C-k") 'cb-sp:kill-blank-lines)

(defvar sp-minibuffer-enabled-commands
  '(eval-expression calc-algebraic-entry quick-calc)
  "Commands that take input in the minibuffer for which smartparens should be used.")

(hook-fns '(minibuffer-setup-hook minibuffer-inactive-mode-hook)
  :append t
  (smartparens-mode (if (-contains? sp-minibuffer-enabled-commands this-command)
                        +1 -1)))

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

(setq sp-navigate-close-if-unbalanced t)

(defun sp-insert-or-up (delim &optional arg)
  "Insert a delimiter DELIM if inside a string, else move up."
  (interactive "sDelimiter:\nP")
  (cond ((or (emr-looking-at-string?) (emr-looking-at-comment?))
         (insert delim))
        (smartparens-mode
         (sp-up-sexp arg 'interactive))
        (t
         (insert delim))))

(defun cbsp:hacky-set-sp-bindings ()
  (cl-loop for key in '(")" "]" "}")
           for map in '(smartparens-mode-map smartparens-strict-mode-map)
           do (eval `(bind-key
                      (kbd key)
                      (command (with-demoted-errors
                                   (sp-insert-or-up ,key _arg)))
                      ,map))))

(add-hook 'smartparens-mode-hook 'cbsp:hacky-set-sp-bindings t)
(add-hook 'smartparens-strict-mode-hook 'cbsp:hacky-set-sp-bindings t)

(after 'evil

  (evil-global-set-key 'normal "(" 'sp-backward-up-sexp)
  (evil-global-set-key 'normal ")" 'sp-forward-sexp)

  (evil-define-state paren "Paren editing state."
    :tag " <P> "
    :message "-- PAREN --"
    :suppress-keymap t
    :cursor 'hollow)

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
    "M" 'sp-backward-slurp-sexp
    "m" 'sp-forward-slurp-sexp
    "c" 'sp-convolute-sexp
    "D" 'sp-backward-kill-sexp
    "d" 'sp-kill-sexp
    "e" 'sp-emit-sexp
    "G" 'sp-end-of-sexp
    "g" 'sp-beginning-of-sexp
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
    "," 'sp-previous-sexp
    "." 'sp-next-sexp
    "<" 'sp-backward-down-sexp
    ">" 'sp-down-sexp))

(after '(smartparens tex)
  (require 'smartparens-latex))

(provide 'config-smartparens)

;;; config-smartparens.el ends here
