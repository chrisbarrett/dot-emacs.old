;;; config-coq.el --- Configure Coq

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

;; Configure Coq

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)
(require 'config-smartparens)

(require 'proof-site (f-join cb:lib-dir "proofgeneral" "generic" "proof-site"))
(add-to-list 'Info-directory-list (f-join cb:lib-dir "proofgeneral" "doc"))

(after 'proof-script
  (define-derived-mode proof-mode prog-mode
    proof-general-name
    "Proof General major mode class for proof scripts.
\\{proof-mode-map}"

    (setq proof-buffer-type 'script)

    ;; Set default indent function (can be overriden in derived modes)
    (make-local-variable 'indent-line-function)
    (setq indent-line-function 'proof-indent-line)

    ;; During write-file it can happen that we re-set the mode for the
    ;; currently active scripting buffer.  The user might also do this
    ;; for some reason.  We could maybe let this pass through, but it
    ;; seems safest to treat it as a kill buffer operation (retract and
    ;; clear spans).  NB: other situations cause double calls to proof-mode.
    (if (eq (current-buffer) proof-script-buffer)
        (proof-script-kill-buffer-fn))

    ;; We set hook functions here rather than in proof-config-done so
    ;; that they can be adjusted by prover specific code if need be.
    (proof-script-set-buffer-hooks)

    ;; Set after change functions
    (proof-script-set-after-change-functions)

    (add-hook 'after-set-visited-file-name-hooks
              'proof-script-set-visited-file-name nil t)

    (add-hook 'proof-activate-scripting-hook 'proof-cd-sync nil t)))

(custom-set-variables
 '(proof-splash-enable nil))

(defvar cb-coq:smart-operator-list
  '("!" "$" "%" "&" "*" "+" "-" "." "/" ":" "<" "=" ">" "?" "@" "^" "|" "~")
  "The list of operators in the Coq language.")

(put 'coq-mode 'smart-operator-alist cb-coq:smart-operator-list)

(defun cb-coq:smart-pipe ()
  "Insert either the pipe chars in an array literal or a smart pipe."
  (interactive)
  (cond ((thing-at-point-looking-at (rx "[]"))
         (insert "|")
         (save-excursion
           (insert "|")))
        (t
         (cb-coq:super-smart-ops-insert "|"))))

(defun cb-coq:set-keys ()
  "Set smart operators for Coq."
  (--each cb-coq:smart-operator-list
    (define-key coq-mode-map (kbd it) (super-smart-ops-make-smart-op it)))

  (define-key coq-mode-map (kbd "!") nil)
  (define-key coq-mode-map (kbd ".") nil)
  (define-key coq-mode-map (kbd "|") 'cb-ocaml:smart-pipe))

(after 'coq (cb-coq:set-keys))

(defun cb-coq:case-start-col ()
  (save-excursion
    (goto-char (line-beginning-position))
    (search-forward "|")
    (1- (current-column))))

(defun cb-coq:newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (insert str))

(defun cb-coq:newline-and-expand-snippet-at-col (predicate col)
  "Insert a new line, find the template matching PREDICATE and insert at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (yas-insert-first-snippet predicate))

(defun cb-coq:rx-start-column (rx)
  (save-excursion
    (goto-char (line-end-position))
    (search-backward-regexp rx (line-beginning-position))
    (current-column)))

(defun cb-coq:meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond

   ;; Insert case after match statement.
   ((s-matches? (rx symbol-start "match" symbol-end) (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (goto-char (max (line-beginning-position) (- (point) 2)))
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) "Inductive") (current-line))
    (cb-coq:newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert dependent type below the current one.
   ((s-matches? (rx bol (* space) "|" (* space) (+ word) (+ space) ":" (+ space))
                (current-line))
    (cb-coq:newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert match case below the current one.
   ((s-matches? (rx bol (* space) "|" (* nonl) "=>") (current-line))
    (cb-coq:newline-and-expand-snippet-at-col
     (C (~ equal "match-case") yas--template-name)
     (cb-coq:case-start-col)))

   ;; Insert any other kind of case below the current one.
   ((s-matches? (rx bol (* space) "|") (current-line))
    (cb-coq:newline-and-insert-at-col (cb-coq:case-start-col) "| "))

   ;; Insert check.
   ((s-matches? (rx bol "Check") (current-line))
    (cb-coq:newline-and-expand-snippet-at-col
     (C (~ equal "Check") yas--template-name)
     (cb-coq:rx-start-column "Check")))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (when (true? evil-mode)
    (evil-insert-state)))

(after 'coq
  (define-key coq-mode-map (kbd "M-RET")   'cb-coq:meta-ret)
  (define-key coq-mode-map (kbd "C-c C-m") 'coq-insert-match))

(sp-with-modes '(coq-mode)
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{" "}" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[" "]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "(" ")" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "'" nil :actions nil))

(after 'proof-script
  (define-key proof-mode-map (kbd "C-<return>") nil))

(defun cb-coq:get-vars-for-intros (text)
  "Get the variable names for an initial intros clause.
TEXT is proof variables in the snippet."
  (let ((match (s-match (rx (* "(")(group (+ nonl)) ":") text)))
    (s-trim (or (cadr match) "terms"))))

(provide 'config-coq)

;;; config-coq.el ends here
