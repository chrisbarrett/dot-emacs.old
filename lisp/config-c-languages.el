;;; config-c-languages.el --- Configure C languages

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

;; Configure C languages

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)

(autoload 'beginning-of-sexp "thingatpt")

(cb:declare-package-installer c-languages
  :match (rx "." (or "c" "cc" "cpp" "h" "hh" "hpp" "m") eol)
  :packages
  (company-c-headers
   google-c-style
   c-eldoc
   clang-format))

(when (executable-find "clang")
  (custom-set-variables
   '(cc-compilers-list (list "clang"))
   '(cc-default-compiler "clang")
   '(cc-default-compiler-options "-fno-color-diagnostics -g")))

(after 'google-c-style
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(after 'cc-mode
  (require 'google-c-style nil t)

  (define-key c-mode-map (kbd "M-q")
    (if (executable-find "clang-format")
        'clang-format-region
      'indent-dwim))

  (defun cb-c:switch-between-header-and-impl ()
    "Switch between a header file and its implementation."
    (interactive)
    (let* ((ext   (if (f-ext? (buffer-file-name) "h") "c" "h"))
           (counterpart (format "%s.%s" (f-no-ext (buffer-file-name)) ext)))
      (if (or (f-file? counterpart)
              (y-or-n-p (format "%s does not exist.  Create it? " counterpart)))
          (find-file counterpart)
        (message "Aborted"))))

  (define-key c-mode-map (kbd "C-c C-a") 'cb-c:switch-between-header-and-impl)

  (defun cb-c:looking-at-flow-control-header? ()
    (thing-at-point-looking-at
     (rx (* nonl) (? ";") (* space)
         (or "if" "when" "while" "for")
         (* nonl)
         "("
         (* (not (any ")"))))))

  (defun cb-c:looking-at-flow-control-keyword? ()
    (thing-at-point-looking-at
     (rx (or (group (or "if" "when" "while" "for") (or (+ space) "("))
             (group (or "do" "else") (* space))))))

  (defun cb-c:looking-at-assignment-right-side? ()
    (save-excursion
      (thing-at-point-looking-at
       (rx "=" (* space)
           ;; Optional casts
           (? (group "(" (* nonl) ")"))
           (* space)))))

  (defun cb-c:looking-at-cast? ()
    (let ((cast (rx

                 (or
                  "return"
                  (any
                   ;; Operator
                   "+" "-" "*" "/" "|" "&" ">" "<"
                   ;; Expression delimiter
                   ";" "[" "{" "(" ")" "="))

                 (* space)

                 ;; Cast and type
                 "(" (* nonl) ")"

                 (* space)))
          )
      (and (thing-at-point-looking-at cast)
           (save-excursion
             (search-backward-regexp cast)
             (not (cb-c:looking-at-flow-control-keyword?))))))

  (defun cb-c:looking-at-struct-keyword? ()
    (save-excursion
      (beginning-of-sexp)
      (thing-at-point-looking-at (rx (or "{" " " "(" ",") "."))))

  (cl-defun cb-c:header-guard-var (&optional (header-file (buffer-file-name)))
    "Return the variable to use in a header guard for HEADER-FILE."
    (format "_%s_H_" (s-upcase (f-filename (f-no-ext header-file)))))

  (defun cb-c:maybe-remove-spaces-after-insertion (pred-regex op-start-regex)
    (when (thing-at-point-looking-at pred-regex)
      (save-excursion
        (let ((back-limit (save-excursion
                            (search-backward-regexp op-start-regex)
                            (point))))
          (while (search-backward-regexp (rx space) back-limit t)
            (delete-horizontal-space)))
        (indent-according-to-mode))))

  (defun cb-c:just-one-space-after-semicolon ()
    (save-excursion
      (when (search-backward-regexp (rx ";" (* space)) (line-beginning-position) t)
        (replace-match "; " nil))))

  (defun c-insert-smart-equals ()
    "Insert an '=' with context-sensitive formatting."
    (interactive)
    (if (or (cb-c:looking-at-flow-control-header?)
            (cb-c:looking-at-struct-keyword?))
        (insert "=")
      (super-smart-ops-insert "=")))

  (defun c-insert-smart-star ()
    "Insert a * with padding in multiplication contexts."
    (interactive)
    (cond
     ((s-matches? (rx bol (* space) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (indent-according-to-mode)
      (insert "*"))
     ((thing-at-point-looking-at (rx (any "(" "{" "[") (* space)))
      (insert "*"))
     ((thing-at-point-looking-at (rx (any digit "*") (* space)))
      (super-smart-ops-insert "*"))
     (t
      (just-one-space)
      (insert "*"))))

  (defun c-insert-smart-minus ()
    "Insert a minus with padding unless a unary minus is more appropriate."
    (interactive)
    (atomic-change-group
      ;; Handle formatting for unary minus.
      (if (thing-at-point-looking-at
           (rx (or "return" "," "(" "[" "(" ";" "=") (* space)))
          (insert "-")
        (super-smart-ops-insert "-"))
      ;; Collapse whitespace for decrement operator.
      (cb-c:maybe-remove-spaces-after-insertion
       (rx "-" (* space) "-" (* space))
       (rx (not (any "-" space))))
      (cb-c:just-one-space-after-semicolon)))

  (defun c-insert-smart-gt ()
    "Insert a > symbol with formatting.
If the insertion creates an right arrow (->), remove surrounding whitespace.
If the insertion creates a <>, move the cursor inside."
    (interactive)
    (super-smart-ops-insert ">")
    (cb-c:maybe-remove-spaces-after-insertion
     (rx (or "-" "<") (* space) ">" (* space))
     (rx (not (any space "<" "-" ">"))))
    (when (thing-at-point-looking-at "<>")
      (forward-char -1)))

  (defun c-insert-smart-plus ()
    "Insert a + symbol with formatting.
Remove horizontal whitespace if the insertion results in a ++."
    (interactive)
    (super-smart-ops-insert "+")
    (cb-c:maybe-remove-spaces-after-insertion
     (rx "+" (* space) "+" (* space))
     (rx (not (any space "+"))))
    (cb-c:just-one-space-after-semicolon))

  (super-smart-ops-configure-for-mode 'c-mode
    :add '("?")
    :custom
    '(("," . cb:comma-then-space)
      ("=" . c-insert-smart-equals)
      ("+" . c-insert-smart-plus)
      (">" . c-insert-smart-gt)
      ("-" . c-insert-smart-minus)
      ("*" . c-insert-smart-star)))

  (defun cbclang:flyspell-verify ()
    (not (s-matches? (rx bol (* space) "#include ") (current-line))))

  (hook-fns '(c-mode-hook c++-mode-hook)
    (setq-local flyspell-generic-check-word-predicate 'cbclang:flyspell-verify))

  (defadvice c-inside-bracelist-p (around ignore-errors activate)
    (ignore-errors ad-do-it))

  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

  (add-hook 'c-mode-hook 'flyspell-mode-off)

  (defun cb-cc:between-empty-braces-same-line? ()
    (and (s-matches? (rx "{" (* space) eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol (* space) "}")
                     (buffer-substring (point) (line-end-position)))))

  (defun cb-cc:newline-and-indent ()
    "Insert newlines, performing context-specific formatting.

When point is between braces, insert an empty line between them so that

{|}

becomes

{
  |
}"
    (interactive)
    (when (cb-cc:between-empty-braces-same-line?)
      (delete-horizontal-space)

      (save-excursion
        (search-backward "{")
        (unless (thing-at-point-looking-at (rx bol (* space) "{"))
          (newline-and-indent)))

      (save-excursion
        (newline-and-indent)))
    (call-interactively 'newline-and-indent))


  (defun cb-cc:delete-brace-contents ()
    (cl-destructuring-bind (&optional &key beg end op &allow-other-keys)
        (sp-get-enclosing-sexp)
      (when (equal op "{")
        (delete-region (1+ beg) (1- end))
        (goto-char (1+ beg)))))

  (defun cb-cc:between-empty-braces-any-lines? ()
    (cl-destructuring-bind (&optional &key beg end op &allow-other-keys)
        (sp-get-enclosing-sexp)
      (when (equal op "{")
        (s-matches? (rx bos (* (any space "\n")) eos)
                    (buffer-substring (1+ beg) (1- end))))))

  (defun cb-cc:backward-delete-char ()
    "Delete backwards, performing context-specific formatting.

When point is between empty braces over any number of lines, collapse them:

{
  |
}

becomes

{ | }

then

{|}"
    (interactive)
    (cond
     ((and (equal (char-before) ?{)
           (equal (char-after) ?}))
      (sp-backward-delete-char))

     ((and (thing-at-point-looking-at (rx "{" (+ space) "}"))
           (cb-cc:between-empty-braces-same-line?))
      (delete-horizontal-space))

     ((cb-cc:between-empty-braces-any-lines?)
      (cb-cc:delete-brace-contents)
      (insert "  ")
      (forward-char -1))

     (t
      (sp-backward-delete-char))))

  (--each (list c-mode-map c++-mode-map java-mode-map objc-mode-map)
    (define-key it (kbd "RET") 'cb-cc:newline-and-indent)
    (define-key it (kbd "<backspace>") 'cb-cc:backward-delete-char))

  )

(after '(cc-mode emr)

  (defun helm-insert-c-header ()
    (interactive)
    (helm :sources
          `((name . "C Headers")
            (candidates . ,(-concat emr-c:standard-headers
                                    (emr-c:headers-in-project)))
            (action .
                    (lambda (c)
                      (emr-c-insert-include
                       (format (if (-contains? emr-c:standard-headers c)
                                   "<%s>"
                                 "\"%s\"")
                               c))
                      (when (derived-mode-p 'bison-mode)
                        (bison-format-buffer))))
            (volatile))
          :prompt "Header: "
          :buffer "*Helm C Headers*"))

  (add-to-list 'insertion-picker-options
               '("i" "Header Include" helm-insert-c-header
                 :modes (c-mode c++-mode))))

(after '(cc-mode smartparens)
  (defun cb-c:format-after-brace (_id action contexxt)
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
    (sp-local-pair "{" "}" :post-handlers '(:add cb-c:format-after-brace))
    (sp-local-pair "(" ")" :post-handlers '(:add cb-c:format-after-paren))))



(provide 'config-c-languages)

;;; config-c-languages.el ends here
