;;; cb-clang.el --- Configuration for C-like languages

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130526.2355

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

;; Configuration for C-like languages

;;; Code:

(require 'use-package)
(require 'dash)
(require 'cb-lib)
(autoload 'c-mode-map "cc-mode")
(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'smartparens-mode-map "smartparens")
(autoload 'beginning-of-sexp "thingatpt")

(after 'cc-mode

  ;; Use clang as the cc compiler.
  (when (executable-find "clang")
    (setq
     cc-compilers-list (list "clang")
     cc-default-compiler "clang"
     cc-default-compiler-options "-fno-color-diagnostics -g"))

  (defun cb-c:switch-between-header-and-impl ()
    "Switch between a header file and its implementation."
    (interactive)
    (let ((sans (file-name-sans-extension (buffer-file-name)))
          (ext  (file-name-extension (buffer-file-name))))
      (if (equal "h" ext)
          (find-file (concat sans ".c"))
        (find-file (concat sans ".h")))))

  (define-key c-mode-map (kbd "C-c C-j") 'cb-c:switch-between-header-and-impl)
  (define-key c-mode-map (kbd "M-q") 'indent-dwim)
  (after 'mode-compile
    (define-key c-mode-map (kbd "C-c C-c") 'mode-compile))

  (require 'smart-operator)
  (require 'flycheck)
  (require 'smartparens)

  (defadvice c-inside-bracelist-p (around ignore-errors activate)
    "Ignore errors thrown by internals."
    (ignore-errors ad-do-it)))

(after 'auto-complete
  (hook-fn 'c-mode-hook
    (setq ac-sources '(ac-source-clang-async
                       ac-source-yasnippet))))

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

(after 'smart-operator

  (defun c-insert-smart-op (str)
    "Insert a smart operator with special formatting in certain expressions."
    (if (cb-c:looking-at-flow-control-header?)
        (insert str)
      (smart-insert-operator str)))

  (defun c-insert-smart-equals ()
    "Insert an '=' with context-sensitive formatting."
    (interactive)
    (if (or (cb-c:looking-at-flow-control-header?)
            (cb-c:looking-at-struct-keyword?))
        (insert "=")
      (smart-insert-operator "=")))

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
      (smart-insert-operator "*"))
     (t
      (just-one-space)
      (insert "*"))))

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

  (defun c-insert-smart-minus ()
    "Insert a minus with padding unless a unary minus is more appropriate."
    (interactive)
    (atomic-change-group
      ;; Handle formatting for unary minus.
      (if (thing-at-point-looking-at
           (rx (or "return" "," "(" "[" "(" ";" "=") (* space)))
          (insert "-")
        (c-insert-smart-op "-"))
      ;; Collapse whitespace for decrement operator.
      (cb-c:maybe-remove-spaces-after-insertion
       (rx "-" (* space) "-" (* space))
       (rx (not (any "-" space))))
      (cb-c:just-one-space-after-semicolon)))

  (defun c-insert-smart-gt ()
    "Insert a > symbol with formatting.
If the insertion creates an right arrow (->), remove surrounding whitespace."
    (interactive)
    (c-insert-smart-op ">")
    (cb-c:maybe-remove-spaces-after-insertion
     (rx "-" (* space) ">" (* space))
     (rx (not (any space "-" ">")))))

  (defun c-insert-smart-plus ()
    "Insert a + symbol with formatting.
Remove horizontal whitespace if the insertion results in a ++."
    (interactive)
    (c-insert-smart-op "+")
    (cb-c:maybe-remove-spaces-after-insertion
     (rx "+" (* space) "+" (* space))
     (rx (not (any space "+"))))
    (cb-c:just-one-space-after-semicolon))

  (hook-fn 'c-mode-hook
    (local-set-key (kbd ",") 'cb:comma-then-space)
    (local-set-key (kbd "=") 'c-insert-smart-equals)
    (local-set-key (kbd "|") (smart-op "|"))
    (local-set-key (kbd "?") (smart-op "?"))
    (local-set-key (kbd ":") (smart-op ":"))
    (local-set-key (kbd "+") 'c-insert-smart-plus)
    (local-set-key (kbd ">") 'c-insert-smart-gt)
    (local-set-key (kbd "-") 'c-insert-smart-minus)
    (local-set-key (kbd "*") 'c-insert-smart-star)))

(after 'smartparens

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

(use-package google-c-style
  :ensure   t
  :defer    t
  :commands google-set-c-style
  :init    (add-hook 'c-mode-common-hook 'google-set-c-style))

(use-package disaster
  :ensure   t
  :commands disaster
  :defer    t
  :init
  (hook-fn 'c-mode-common-hook
    (local-set-key (kbd "C-c C-d") 'disaster)))

(use-package auto-complete-clang-async
  :commands ac-clang-launch-completion-process
  :init
  (hook-fn 'c-mode-common-hook
    (setq ac-sources '(ac-source-clang-async
                       ac-source-yasnippet
                       ac-source-words-in-buffer))
    (ac-clang-launch-completion-process))
  :config
  (progn
    (copy-face 'ac-candidate-face 'ac-clang-candidate-face)
    (copy-face 'ac-selection-face 'ac-clang-selection-face)
    (define-path cb:clang-complete-dir "lib/clang-complete-async/")
    (setq ac-clang-complete-executable (concat cb:clang-complete-dir "clang-complete"))))

(use-package ac-c-headers
  :ensure t
  :defer t
  :init
  (hook-fn 'c-mode-hook
    (require 'ac-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

(use-package c-eldoc
  :ensure   t
  :commands c-turn-on-eldoc-mode
  :init     (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(provide 'cb-clang)

;; Local Variables:
;; End:

;;; cb-clang.el ends here
