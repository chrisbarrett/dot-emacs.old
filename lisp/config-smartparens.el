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
(require 'config-modegroups)

(cb:install-package 'smartparens t)
(require 'smartparens-config)
(require 'smartparens-latex)

;;; Enable smartparens

(smartparens-global-mode)
(show-smartparens-global-mode +1)

(add-hook 'prog-mode-hook       'smartparens-strict-mode)
(add-hook 'cb:markup-modes-hook 'smartparens-strict-mode)
(add-hook 'eshell-mode-hook     'smartparens-mode)

(custom-set-variables
 '(sp-autoinsert-if-followed-by-word t)
 '(sp-navigate-close-if-unbalanced t))

(diminish 'smartparens-mode)

;;; Use smartparens in certain minibuffer contexts.

(defvar sp-minibuffer-enabled-commands
  '(eval-expression calc-algebraic-entry quick-calc)
  "Commands that take input in the minibuffer for which smartparens should be used.")

(hook-fns '(minibuffer-setup-hook minibuffer-inactive-mode-hook)
  :append t
  (smartparens-mode (if (-contains? sp-minibuffer-enabled-commands this-command)
                        +1 -1)))

;;; Move up and reformat parens when closing.

(defun sp-insert-or-up (delim &optional arg)
  "Insert a delimiter DELIM if inside a string, else move up.
Prefix ARG is passed to `sp-up-sexp'."
  (interactive "sDelimiter:\nP")
  (let ((in-string-or-comment? (nth 8 (syntax-ppss))))
    (cond (in-string-or-comment?
           (insert delim))
          (smartparens-mode
           (sp-up-sexp arg 'interactive))
          (t
           (insert delim)))))

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

;;; Utility commands

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

(defun cbsp:internal-and-external-padding (id action context)
  "Insert internal and external padding."
  (and (cbsp:external-padding id action context)
       (cbsp:internal-padding id action context)))

(defun cbsp:external-padding (id action ctx)
  "Add external padding around ID.
Insert leading padding unless at start of line or after an open round paren."
  (when (and (equal action 'insert)
             (equal ctx 'code))
    (save-excursion
      (when (search-backward (sp-get-pair id :open)
                             (line-beginning-position) t)
        (let ((bol-to-point (buffer-substring (line-beginning-position) (point))))
          (cond
           ((s-matches? (rx bol (* space) eol) bol-to-point))
           ((s-matches? (rx "(" (* space) eol) bol-to-point)
            (delete-horizontal-space))
           (t
            (just-one-space))))
        t))))

(defun cbsp:internal-padding (id action ctx)
  "Add internal padding around ID."
  (when (and (equal action 'insert)
             (equal ctx 'code))
    (when (search-backward (sp-get-pair id :open)
                           (line-beginning-position) t)

      (goto-char (match-end 0))
      (insert "  ")
      (forward-char -1))))

;;; Remove apostrophe pair for some modes

(sp-local-pair cb:prompt-modes           "'" "'" :actions '(:rem insert))
(sp-local-pair 'org-mode                 "'" "'" :actions '(:rem insert))
(sp-local-pair 'text-mode                "'" "'" :actions '(:rem insert))
(sp-local-pair 'minibuffer-inactive-mode "'" "'" :actions '(:rem insert))

;;; Rust

(sp-with-modes 'rust-mode
  (sp-local-pair "{" "}" :post-handlers '(:add cbsp:internal-and-external-padding))
  (sp-local-pair "'" "'" :actions '(:rem insert))
  )

;;; Haskell

(sp-with-modes cb:haskell-modes
  (sp-local-pair "{" "}" :post-handlers '(:add cbsp:internal-and-external-padding))
  (sp-local-pair "(" ")" :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "[" "]" :post-handlers '(:add cbsp:external-and-external-padding))
  (sp-local-pair "`" "`" :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "'" "'" :actions '(:rem insert))
  )

;;; OCaml

(defun cb-ocaml:just-inserted-double-quotes? (id action ctx)
  (and (sp-in-string-p id action ctx)
       (s-matches? (rx (not (any "\\")) "\"" eol)
                   (buffer-substring (line-beginning-position) (point)))))

(defun sp-ocaml-just-one-space (id action ctx)
  "Pad delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (cb-ocaml:just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is the first position of another list
    ;; 2. this form begins a new line.
    ;; 3. this form is preceded by a `?`, as in a let binding.
    ;; 4. this form is preceded by a `:`, as in a keyword argument
    ;; 4. this form is preceded by a `.`, as in an array index expression
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "." "," ":" "(" "[" "[|" "{" "?")
                       ;; HACK: utop prompt
                       (and "utop[" (+ digit) "]" ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (s-matches? (rx (or (any ")" "]" "|]" "}") eol))
                          (char-to-string (char-after)))
        (just-one-space)))))

(sp-with-modes '(tuareg-mode utop-mode)
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{" "}"   :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[" "]"   :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "(" ")"   :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[|" "|]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{<" ">}" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "'" "'"   :actions '(:rem insert))
  (sp-local-pair "`" nil   :actions nil)
  )

;;; Coq

(sp-with-modes 'coq-mode
  (sp-local-pair "\"" "\"" :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "{" "}"   :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "[" "]"   :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "(" ")"   :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "'" "'"   :actions '(:rem insert))
  )

;;; F#

(sp-with-modes 'fsharp-mode
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{" "}"   :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[" "]"   :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "(" ")"   :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[|" "|]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[<" ">]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "'" "'"   :actions '(:rem insert))
  (sp-local-pair "`" nil   :actions nil)
  )

;;; Idris

(sp-with-modes cb:idris-modes
  (sp-local-pair "\"" "\"" :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "{" "}"   :post-handlers '(:add cbsp:internal-and-external-padding))
  (sp-local-pair "[" "]"   :post-handlers '(:add cbsp:internal-and-external-padding))
  (sp-local-pair "(" ")"   :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "`" "`"   :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "[|" "|]" :post-handlers '(:add cbsp:external-padding))
  (sp-local-pair "'" nil   :actions nil)
  (sp-local-pair "'" "'"   :actions '(:rem insert))
  )

;;; Lisp modes

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
      (unless (-contains? '(")" "]" "}") (char-to-string (char-after)))
        (just-one-space)))))

(sp-with-modes cb:lisp-modes
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "{" "}"   :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "[" "]"   :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "(" ")"   :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "'" nil   :actions nil)
  )

;; Define a backtick pair for all non-elisp modes.
(sp-local-pair (-difference cb:lisp-modes cb:elisp-modes)
               "`" "`" :when '(sp-in-string-p))

;; Extend `sp-navigate-reindent-after-up' to all lisps.
(let ((ls (assoc 'interactive sp-navigate-reindent-after-up)))
  (setcdr ls (-uniq (-concat (cdr ls) cb:lisp-modes))))

(add-hook 'ielm-mode-hook 'smartparens-strict-mode)

;;; Markdown

(sp-with-modes 'markdown-mode
  (sp-local-pair "```" "```"))

;;; Python

(sp-with-modes cb:python-modes
  (sp-local-pair "{" "}" :post-handlers '(:add cbsp:external-padding)))

;;; Ruby

(require 'smartparens-ruby)

(after 'ruby-mode
  (modify-syntax-entry ?@ "w" ruby-mode-syntax-table)
  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
  (modify-syntax-entry ?! "w" ruby-mode-syntax-table)
  (modify-syntax-entry ?? "w" ruby-mode-syntax-table))

(defun sp-ruby-should-insert-pipe-close (_id _action _ctx)
  "Test whether to insert the closing pipe for a lambda-binding pipe pair."
  (thing-at-point-looking-at
   (rx-to-string `(and (or "do" "{") (* space) "|"))))

(defun sp-ruby-sp-hook-space-before (_id action _ctx)
  "Move to point before ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-backward "|")
      (just-one-space))))

(defun sp-ruby-sp-hook-space-after (_id action _ctx)
  "Move to point after ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-forward "|")
      (just-one-space))))

(sp-with-modes cb:ruby-modes
  (sp-local-pair "{" "}" :post-handlers '(:add cbsp:internal-and-external-padding))
  (sp-local-pair "[" "]" :pre-handlers '(sp-ruby-pre-handler))

  (sp-local-pair "%q{" "}" :when '(sp-in-code-p))
  (sp-local-pair "%Q{" "}" :when '(sp-in-code-p))
  (sp-local-pair "%w{" "}" :when '(sp-in-code-p))
  (sp-local-pair "%W{" "}" :when '(sp-in-code-p))
  (sp-local-pair  "%(" ")" :when '(sp-in-code-p))
  (sp-local-pair "%x(" ")" :when '(sp-in-code-p))
  (sp-local-pair  "#{" "}" :when '(sp-in-string-p))

  (sp-local-pair "|" "|"
                 :when '(sp-ruby-should-insert-pipe-close)
                 :unless '(sp-in-string-p)
                 :pre-handlers '(sp-ruby-sp-hook-space-before)
                 :post-handlers '(sp-ruby-sp-hook-space-after))

  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler)))

;;; Swift

(sp-with-modes '(swift-mode)
  (sp-local-pair "{" "}" :post-handlers '(:add cbsp:internal-and-external-padding))
  (sp-local-pair "'" "'"   :actions '(:rem insert))
  )

;;; C

(defun cb-c:format-after-brace (_id action context)
  "Apply formatting after a brace insertion."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               ;; Search backward for flow control keywords.
               (search-backward "{")
               (or (thing-at-point-looking-at
                    (rx symbol-start (or "else" "do")))
                   (progn
                     (sp-previous-sexp)
                     (thing-at-point-looking-at
                      (rx symbol-start (or "if" "for" "while")))))))
    ;; Insert a space for padding.
    (save-excursion
      (search-backward "{")
      (just-one-space))
    ;; Put braces on new line.
    (newline)
    (save-excursion (newline-and-indent))
    (c-indent-line)))

(defun cb-c:format-after-paren (_id action context)
  "Insert a space after flow control keywords."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               (search-backward "(")
               (thing-at-point-looking-at
                (rx symbol-start (or "=" "return" "if" "while" "for")
                    (* space)))))
    (save-excursion
      (search-backward "(")
      (just-one-space))))

(sp-with-modes '(c-mode cc-mode c++-mode)
  (sp-local-pair "{" "}" :post-handlers '(:add cb-c:format-after-brace
                                               cbsp:external-and-external-padding))
  (sp-local-pair "(" ")" :post-handlers '(:add cb-c:format-after-paren)))

;;; Key bindings

(sp-pair "(" ")"   :bind "M-(")
(sp-pair "{" "}"   :bind "M-{")
(sp-pair "[" "]"   :bind "M-[")
(sp-pair "\"" "\"" :bind "M-\"")
(sp-pair "`" "`"   :bind "M-`")

(define-key sp-keymap (kbd "C-k") 'cb-sp:kill-blank-lines)
(define-key sp-keymap (kbd "DEL") 'sp-backward-delete-char)

;; Use bind-key for keys that tend to be overridden.
(bind-key "C-M-," 'sp-backward-down-sexp sp-keymap)
(bind-key "C-M-." 'sp-next-sexp sp-keymap)


(provide 'config-smartparens)

;;; config-smartparens.el ends here
