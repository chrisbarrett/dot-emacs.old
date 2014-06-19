;;; config-scheme.el --- Configure scheme

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

;; Configure scheme

;;; Code:


(require 'utils-common)
(require 'config-modegroups)
(require 'config-evil)

(custom-set-variables
 '(geiser-mode-start-repl-p t)
 '(geiser-repl-startup-time 20000)
 '(geiser-repl-history-filename (f-join cb:tmp-dir "geiser-history"))
 '(geiser-active-implementations '(racket)))


(after 'evil
  (define-evil-doc-handler cb:scheme-modes
    (call-interactively 'geiser-doc-symbol-at-point)))

(after 'scheme
  (cb:install-package 'geiser))

(after 'geiser-mode
  (defun geiser-eval-buffer (&optional and-go raw nomsg)
    "Eval the current buffer in the Geiser REPL.

With prefix, goes to the REPL buffer afterwards (as
`geiser-eval-buffer-and-go')"
    (interactive "P")
    (let ((start (progn
                   (goto-char (point-min))
                   (while (s-matches? (rx bol "#") (current-line))
                     (forward-line))
                   (point)))
          (end (point-max)))
      (save-restriction
        (narrow-to-region start end)
        (check-parens))
      (geiser-debug--send-region nil
                                 start
                                 end
                                 (and and-go 'geiser--go-to-repl)
                                 (not raw)
                                 nomsg)))

  (define-key geiser-mode-map (kbd "C-c C-f") 'geiser-eval-buffer)
  (define-key geiser-mode-map (kbd "C-c C-h") 'geiser-doc-look-up-manual)
  (define-key geiser-repl-mode-map (kbd "C-c C-h") 'geiser-doc-look-up-manual)

  (defadvice switch-to-geiser (after append-with-evil activate)
    (when (derived-mode-p 'comint-mode)
      (goto-char (point-max))))

  (after 'evil
    (evil-define-key 'normal geiser-mode-map
      (kbd "M-.") 'geiser-edit-symbol-at-point))

  (after 'evil
    (defadvice switch-to-geiser (after append-with-evil activate)
      (when (derived-mode-p 'comint-mode)
        (cb:maybe-evil-insert-state))))

  )

(defconst cbscm:scm-buf "*execute scheme*")

(defun cbscm:lang (s)
  (or (cadr (s-match (rx bol "#lang" (+ space) (group (+ nonl))) s))
      "racket"))

(defun cbscm:run-file (file language)
  (interactive "f")
  (start-process cbscm:scm-buf cbscm:scm-buf
                 "racket" "-I" language file))

(defun cbscm:execute-buffer ()
  "Compile and run the current buffer in Racket."
  (interactive)
  ;; Kill running processes and prepare buffer.
  (with-current-buffer (get-buffer-create cbscm:scm-buf)
    (read-only-mode +1)
    (ignore-errors (kill-process))
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))))

  ;; Start a new Scheme process in the appropriate language for this file.
  (let ((lang (cbscm:lang (buffer-string))))
    (cond
     ;; Create a temp file if there are unwritten changes or this buffer does
     ;; not have a corresponding file.
     ((or (buffer-modified-p)
          (and (buffer-file-name) (not (f-exists? (buffer-file-name)))))
      (let ((f (make-temp-file nil nil ".rkt")))
        (f-write (buffer-string) 'utf-8 f)
        (cbscm:run-file f lang)))
     ;; Otherwise run this file directly.
     (t
      (cbscm:run-file (buffer-file-name) lang))))

  (display-buffer-other-frame cbscm:scm-buf))

(after 'scheme
  (define-key scheme-mode-map (kbd "C-c C-c") 'cbscm:execute-buffer))

(after 'scheme
  (put 'begin                 'scheme-indent-function 0)
  (put 'begin-for-syntax      'scheme-indent-function 0)
  (put 'case                  'scheme-indent-function 1)
  (put 'cond                  'scheme-indent-function 0)
  (put 'delay                 'scheme-indent-function 0)
  (put 'do                    'scheme-indent-function 2)
  (put 'lambda                'scheme-indent-function 1)
  (put 'λ                     'scheme-indent-function 1)
  (put 'lambda:               'scheme-indent-function 1)
  (put 'case-lambda           'scheme-indent-function 0)
  (put 'lambda/kw             'scheme-indent-function 1)
  (put 'define/kw             'scheme-indent-function 'defun)
  (put 'let                   'scheme-indent-function 'scheme-let-indent)
  (put 'let*                  'scheme-indent-function 1)
  (put 'letrec                'scheme-indent-function 1)
  (put 'let-values            'scheme-indent-function 1)
  (put 'let*-values           'scheme-indent-function 1)
  (put 'fluid-let             'scheme-indent-function 1)
  (put 'let/cc                'scheme-indent-function 1)
  (put 'let/ec                'scheme-indent-function 1)
  (put 'let-id-macro          'scheme-indent-function 2)
  (put 'let-macro             'scheme-indent-function 2)
  (put 'letmacro              'scheme-indent-function 1)
  (put 'letsubst              'scheme-indent-function 1)
  (put 'sequence              'scheme-indent-function 0) ; SICP, not r4rs
  (put 'letsyntax             'scheme-indent-function 1)
  (put 'let-syntax            'scheme-indent-function 1)
  (put 'letrec-syntax         'scheme-indent-function 1)
  (put 'syntax-rules          'scheme-indent-function 1)
  (put 'syntax-id-rules       'scheme-indent-function 1)

  (put 'call-with-input-file  'scheme-indent-function 1)
  (put 'call-with-input-file* 'scheme-indent-function 1)
  (put 'with-input-from-file  'scheme-indent-function 1)
  (put 'with-input-from-port  'scheme-indent-function 1)
  (put 'call-with-output-file 'scheme-indent-function 1)
  (put 'call-with-output-file* 'scheme-indent-function 1)
  (put 'with-output-to-file   'scheme-indent-function 'defun)
  (put 'with-output-to-port   'scheme-indent-function 1)
  (put 'with-slots            'scheme-indent-function 2)
  (put 'with-accessors        'scheme-indent-function 2)
  (put 'call-with-values      'scheme-indent-function 2)
  (put 'dynamic-wind          'scheme-indent-function 'defun)

  (put 'if                    'scheme-indent-function 1)
  (put 'method                'scheme-indent-function 1)
  (put 'beforemethod          'scheme-indent-function 1)
  (put 'aftermethod           'scheme-indent-function 1)
  (put 'aroundmethod          'scheme-indent-function 1)
  (put 'when                  'scheme-indent-function 1)
  (put 'unless                'scheme-indent-function 1)
  (put 'thunk                 'scheme-indent-function 0)
  (put 'while                 'scheme-indent-function 1)
  (put 'until                 'scheme-indent-function 1)
  (put 'parameterize          'scheme-indent-function 1)
  (put 'parameterize*         'scheme-indent-function 1)
  (put 'syntax-parameterize   'scheme-indent-function 1)
  (put 'with-handlers         'scheme-indent-function 1)
  (put 'with-handlers*        'scheme-indent-function 1)
  (put 'begin0                'scheme-indent-function 1)
  (put 'with-output-to-string 'scheme-indent-function 0)
  (put 'ignore-errors         'scheme-indent-function 0)
  (put 'no-errors             'scheme-indent-function 0)
  (put 'matcher               'scheme-indent-function 1)
  (put 'match                 'scheme-indent-function 1)
  (put 'regexp-case           'scheme-indent-function 1)
  (put 'dotimes               'scheme-indent-function 1)
  (put 'dolist                'scheme-indent-function 1)

  (put 'with-syntax           'scheme-indent-function 1)
  (put 'syntax-case           'scheme-indent-function 2)
  (put 'syntax-case*          'scheme-indent-function 3)
  (put 'syntax-parse          'scheme-indent-function 1)
  (put 'module                'scheme-indent-function 2)

  (put 'syntax                'scheme-indent-function 0)
  (put 'quasisyntax           'scheme-indent-function 0)
  (put 'syntax/loc            'scheme-indent-function 1)
  (put 'quasisyntax/loc       'scheme-indent-function 1)

  (put 'cases                 'scheme-indent-function 1)

  (put 'for                   'scheme-indent-function 1)
  (put 'for*                  'scheme-indent-function 1)
  (put 'for/list              'scheme-indent-function 1)
  (put 'for*/list             'scheme-indent-function 1)
  (put 'for/fold              'scheme-indent-function 2)
  (put 'for*/fold             'scheme-indent-function 2)
  (put 'for/and               'scheme-indent-function 1)
  (put 'for*/and              'scheme-indent-function 1)
  (put 'for/or                'scheme-indent-function 1)
  (put 'for*/or               'scheme-indent-function 1)

  (put 'nest                  'scheme-indent-function 1))

(after 'scheme
  (--each cb:scheme-modes
    (font-lock-add-keywords
     it
     `(;; Special forms in Typed Racket.
       (,(rx "("
             (group (or
                     ;; let family
                     (and (? "p") "let" (* (syntax word)) ":")
                     (and "let/" (+ (syntax word)))
                     ;; lambdas
                     (and (* (syntax word)) "lambda:")
                     ;; loops
                     (and "for" (* (syntax word)) ":")
                     "do:"
                     ;; Types
                     "struct:"
                     ":"
                     "provide:"
                     "cast"))

             eow)
        (1 font-lock-keyword-face))

       ;; Definition forms
       (,(rx "(" (group "def" (* (syntax word)) eow))
        (1 font-lock-keyword-face))

       ;; Bindings created by `define-values'
       (,(rx "(define-values" (+ space)
             "(" (group (+ (or (syntax word) space))) ")")
        (1 font-lock-variable-name-face))

       ;; General binding identifiers
       (,(rx "(def" (* (syntax word)) (+ space)
             (group (+ (syntax word))))
        (1 font-lock-variable-name-face))

       ;; Function identifiers
       (,(rx "(def" (* (syntax word)) (+ space)
             "(" (group (+ (syntax word))))
        (1 font-lock-function-name-face))

       ;; Function identifier in type declaration
       (,(rx "(:" (+ space) (group bow (+ (syntax word)) eow))
        (1 font-lock-function-name-face))

       ;; Arrows
       (,(rx bow "->" eow)
        (0 (prog1 nil (compose-region (match-beginning 0) (match-end 0) "→"))))

       ;; Match keywords
       (,(rx "(" (group "match" (* (syntax word)) eow))
        (1 font-lock-keyword-face))

       ;; Error signalling keywords
       (,(rx "(" (group (or "error" "raise")
                        (* (syntax word)) eow))
        (1 font-lock-warning-face))

       ;; Grab-bag of keywords
       (,(rx "(" (group (or (and "begin" num)
                            "parameterize"
                            ))
             eow)
        (1 font-lock-keyword-face))))

    ;; Do not add type font locking to the REPL, because it has too many false
    ;; positives.
    (font-lock-add-keywords
     'scheme-mode
     ;; Types for Typed Racket.
     `((,(rx bow upper (* (syntax word)) eow)
        (0 font-lock-type-face))))))

(provide 'config-scheme)

;;; config-scheme.el ends here
