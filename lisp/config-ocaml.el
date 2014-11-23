;;; config-ocaml.el --- Configure OCaml

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

;; Configure OCaml

;;; Code:

(require 'utils-common)
(require 'config-modegroups)
(require 'super-smart-ops)

(require 'ocp-indent (f-join cb:lib-dir "ocp-indent.el"))

(cb:declare-package-installer ocaml
  :match (rx ".ml" (? (any "y" "i" "l" "p")))
  :packages (tuareg
             merlin))

(custom-set-variables
 '(merlin-default-flags "-w @A-4-33-41-42-43-34-44"))

(add-to-list 'face-remapping-alist '(merlin-type-face . cb:bg-flash))

(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)

(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)

;;; Set OPAM environment vars.

(when (executable-find "opam")
  (cl-loop with env = (read-from-string (shell-command-to-string "opam config env --sexp"))
           for (var val) in (car env)
           do (setenv var val)))

(push (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../share/emacs/site-lisp")
      load-path)

;;; Snippet utils.

(defun cb-ocaml:at-prompt-bol? ()
  (s-matches? (rx bol "utop[" (+ digit) "]>" (* space) (* word) eol)
              (buffer-substring (line-beginning-position) (point))))

;;; Use OCP to indent buffer.

(defun ocp-indent-dwim ()
  "Perform a context-sensitive indentation command."
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively 'ocp-indent-region)
    (message "ocp: Indented region"))
   ((thing-at-point 'defun)
    (ocp-indent-region (save-excursion (beginning-of-defun) (point))
                       (save-excursion (end-of-defun) (point)))
    (message "ocp: Indented defun"))
   (t
    (ocp-indent-region (point-min) (point-max))
    (message "ocp: Indented buffer"))))

(after 'tuareg )

(defun cb-ocaml:maybe-pad-parens ()
  (save-excursion
    (unless (equal (string-to-char " ") (char-before))
      (forward-char -1)
      (when (-contains? '(?\) ?\]) (char-before))
        (just-one-space)))))

(hook-fns '(tuareg-mode-hook utop-mode-hook)
  (add-hook 'post-self-insert-hook 'cb-ocaml:maybe-pad-parens nil t))

;;; Define smart ops.

(defun cb-ocaml:smart-dot ()
  "Smart period for OCaml."
  (interactive)
  (yas-with-field-restriction
    (delete-horizontal-space 'back)
    (insert ".")
    (when (thing-at-point-looking-at (rx (any "+" "/" "-" "*") "."))
      (just-one-space))))

(defun cb-ocaml:smart-colon ()
  "Insert a context-sensitive smart colon."
  (interactive)
  (cond
   ;; Application of named parameter.
   ((thing-at-point-looking-at (rx (or "?" "~") (+ word)))
    (insert ":"))
   ;; Named parameter in val binding.
   ((and (s-matches? (rx bol (* space) "val" symbol-end)
                     (current-line))
         (save-excursion
           (search-backward "->" (line-beginning-position) t)))
    (delete-horizontal-space 'back)
    (insert ":"))
   (t
    (super-smart-ops-insert ":"))))

(defun cb-ocaml:smart-asterisk ()
  "Smart asterisk for OCaml."
  (interactive)
  (yas-with-field-restriction

    (cond
     ;; Additional star for docstring comments of the type:
     ;;
     ;;    (*| *) -> (** | *)
     ;;
     ((s-matches? (rx "(" (+ "*") (* space) eol) (buffer-substring
                                                  (line-beginning-position)
                                                  (point)))
      (delete-horizontal-space 'back)
      (insert "*")
      (just-one-space 2)
      (forward-char -1))
     (t
      (super-smart-ops-insert "*")))))

(defun cb-ocaml:smart-semicolon ()
  "Smart semicolon for OCaml."
  (interactive)
  (yas-with-field-restriction
    (delete-horizontal-space 'back)
    (insert ";")
    (unless (thing-at-point-looking-at ";;")
      (just-one-space))))

(defun cb-ocaml:smart-pipe ()
  "Insert either the pipe chars in an array literal or a smart pipe."
  (interactive)
  (cond ((thing-at-point-looking-at (rx "[]"))
         (insert "|")
         (save-excursion
           (insert "|")))
        (t
         (super-smart-ops-insert "|"))))

(defun cb-ocaml:smart-tilde ()
  "Tilde smart operator. Should not be padded in binding forms."
  (interactive "*")
  (super-smart-ops-insert "~")
  (when (thing-at-point-looking-at (rx space))
    (delete-horizontal-space 'back)))

(defun cb-ocaml:smart-question ()
  "Tilde smart operator. Should not be padded in binding forms."
  (interactive "*")
  (super-smart-ops-insert "?")
  (when (thing-at-point-looking-at (rx space))
    (delete-horizontal-space 'back)))

(super-smart-ops-configure-for-mode 'tuareg-mode
  :add '("$" "@" "^")
  :rem '("!")
  :custom
  '(("*" . cb-ocaml:smart-asterisk)
    ("." . cb-ocaml:smart-dot)
    ("|" . cb-ocaml:smart-pipe)
    (":" . cb-ocaml:smart-colon)
    (";" . cb-ocaml:smart-semicolon)
    ("?" . cb-ocaml:smart-question)
    ("~" . cb-ocaml:smart-tilde)))

(super-smart-ops-configure-for-mode 'utop-mode
  :add '("$" "@" "^")
  :rem '("!")
  :custom
  '(("*" . cb-ocaml:smart-asterisk)
    ("." . cb-ocaml:smart-dot)
    ("|" . cb-ocaml:smart-pipe)
    (":" . cb-ocaml:smart-colon)
    (";" . cb-ocaml:smart-semicolon)
    ("?" . cb-ocaml:smart-question)
    ("~" . cb-ocaml:smart-tilde)))

;;; Smart M-RET

(defun cb-ocaml:newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (insert str))

(defun cb-ocaml:newline-and-expand-snippet-at-col (predicate col)
  "Insert a new line, find the template matching PREDICATE and insert at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (yas-insert-first-snippet predicate))

(defun cb-ocaml:case-start-col ()
  (save-excursion
    (goto-char (line-beginning-position))
    (search-forward "|")
    (1- (current-column))))

(defun cb-ocaml:meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond
   ;; Insert case after function keyword.
   ((s-matches? (rx symbol-start "function" (* space) eol) (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert case after match statement.
   ((s-matches? (rx symbol-start "match" symbol-end) (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) "type") (current-line))
    (cb-ocaml:newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (+ 2 (current-column)))
     "| "))

   ;; Insert line starting with pipeline below the current one.
   ((s-matches? (rx bol (* space) "|>") (current-line))
    (cb-ocaml:newline-and-insert-at-col (cb-ocaml:case-start-col) "|> "))

   ;; Insert match case below the current one.
   ((s-matches? (rx bol (* space) "|" (* nonl) "->") (current-line))
    (cb-ocaml:newline-and-expand-snippet-at-col
     (C (~ equal "match-case") yas--template-name)
     (cb-ocaml:case-start-col)))

   ;; Insert any other kind of case below the current one.
   ((s-matches? (rx bol (* space) "|") (current-line))
    (cb-ocaml:newline-and-insert-at-col (cb-ocaml:case-start-col) "| "))

   ;; Insert new val binding below the current one.
   ((s-matches? (rx bol (* space) "val" symbol-end) (current-line))
    (let ((col (save-excursion
                 (goto-char (line-beginning-position))
                 (forward-to-indentation 0)
                 (current-column))))

      (goto-char (line-end-position))
      (newline)
      (indent-to col)
      (yas-insert-first-snippet (C (~ equal "val") yas--template-name))))

   ;; Insert new let...in binding below the current one.
   ((s-matches? (rx (group "try") symbol-end (* nonl) "with" (* space) eol)
                (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new let...in binding below the current one.
   ((s-matches? (rx bol (* space) "with" (* space) "\n"
                    (* nonl) "->")
                (buffer-substring (save-excursion (forward-line -1)
                                                  (line-beginning-position))
                                  (line-end-position)))
    ;; Insert pipe for case if needed.
    (back-to-indentation)
    (unless (thing-at-point-looking-at "|")
      (insert "| "))
    ;; Insert case.
    (goto-char (line-end-position))
    (newline-and-indent)
    (forward-char -2)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new let...in binding below the current one.
   ((s-matches? (rx bol (* space) (group "let") symbol-end (* nonl) "in" (* space) eol)
                (current-line))
    (cb-ocaml:newline-and-expand-snippet-at-col
     (C (~ equal "let...in") yas--template-name)
     (save-excursion
       (or (search-backward-regexp
            (rx symbol-start "let" symbol-end)
            (line-beginning-position) t)
           (goto-char (line-beginning-position)))
       (current-column))))

   ;; Insert new top-level let binding below the current one.
   ((s-matches? (rx bol (* space) (group "let") symbol-end) (current-line))
    (cb-ocaml:newline-and-expand-snippet-at-col
     (C (~ equal "let (top-level)") yas--template-name)
     (save-excursion
       (or (search-backward-regexp
            (rx symbol-start "let" symbol-end)
            (line-beginning-position) t)
           (goto-char (line-beginning-position)))
       (current-column))))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (when (true? evil-mode)
    (evil-insert-state)))

;;; Repl switching

(defun cb-ocaml:switch-to-utop ()
  (interactive)
  (unless (get-buffer "*utop*")
    (save-window-excursion (utop)))
  (pop-to-buffer "*utop*")
  (goto-char (point-max))
  (when (true? evil-mode)
    (evil-insert-state)))

(defun cb-ocaml:switch-to-src ()
  (interactive)
  (-if-let (buf (--first-buffer (derived-mode-p 'tuareg-mode)))
      (pop-to-buffer buf)
    (message "No active OCaml buffers")))

;;; Key bindings

(after 'merlin
  (define-key merlin-mode-map (kbd "M-N") 'merlin-error-next))

(after 'utop
  (define-key utop-mode-map (kbd "C-c C-z") 'cb-ocaml:switch-to-src)
  (define-key utop-mode-map   (kbd "M-RET") 'cb-ocaml:meta-ret))

(after 'tuareg
  (define-key tuareg-mode-map (kbd "C-c C-z") 'cb-ocaml:switch-to-utop)
  (define-key tuareg-mode-map (kbd "C-c C-f") 'tuareg-eval-buffer)
  (define-key tuareg-mode-map (kbd "C-c C-r") 'tuareg-eval-region)
  (define-key tuareg-mode-map (kbd "C-c C-c") 'tuareg-eval-phrase)
  (define-key tuareg-mode-map (kbd "M-RET") 'cb-ocaml:meta-ret)
  (define-key tuareg-mode-map (kbd "M-q") 'ocp-indent-dwim))

(provide 'config-ocaml)

;;; config-ocaml.el ends here
