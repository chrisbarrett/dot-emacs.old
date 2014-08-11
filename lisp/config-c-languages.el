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

;;; Configure google-c-style.

(autoload  'google-set-c-style "google-c-style")
(autoload  'google-make-newline-indent "google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;; Utils

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

(defun cb-cc:delete-brace-contents ()
  (cl-destructuring-bind (&optional &key beg end op &allow-other-keys)
      (sp-get-enclosing-sexp)
    (when (equal op "{")
      (delete-region (1+ beg) (1- end))
      (goto-char (1+ beg)))))

(defun cb-cc:between-empty-braces-same-line? ()
  (and (s-matches? (rx "{" (* space) eol)
                   (buffer-substring (line-beginning-position) (point)))
       (s-matches? (rx bol (* space) "}")
                   (buffer-substring (point) (line-end-position)))))

(defun cb-cc:between-empty-braces-any-lines? ()
  (cl-destructuring-bind (&optional &key beg end op &allow-other-keys)
      (sp-get-enclosing-sexp)
    (when (equal op "{")
      (s-matches? (rx bos (* (any space "\n")) eos)
                  (buffer-substring (1+ beg) (1- end))))))

;;; Smart ops

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

;;; Flyspell

(defun cbclang:flyspell-verify ()
  (not (s-matches? (rx bol (* space) "#include ") (current-line))))

(put 'c-mode 'flyspell-generic-check-word-predicate 'cbclang:flyspell-verify)
(put 'c++-mode 'flyspell-generic-check-word-predicate 'cbclang:flyspell-verify)

(add-hook 'c-mode-hook 'flyspell-mode-off)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;; Advices

(defadvice c-inside-bracelist-p (around ignore-errors activate)
  (ignore-errors ad-do-it))

;;; Commands

(defun cb-c:format-buffer ()
  "Format the buffer with clang-format, where available."
  (interactive)
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

;;; Key bindings

(after 'cc-mode
  (define-key c-mode-map (kbd "C-c C-a")     'cb-c:switch-between-header-and-impl)
  (define-key c-mode-map (kbd "M-q")         'cb-c:format-buffer)
  (define-key c-mode-map (kbd "RET")         'cb-cc:newline-and-indent)
  (define-key c-mode-map (kbd "<backspace>") 'cb-cc:backward-delete-char))

(provide 'config-c-languages)

;;; config-c-languages.el ends here
