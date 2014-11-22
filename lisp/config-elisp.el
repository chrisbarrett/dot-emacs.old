;;; config-elisp.el --- Configure elisp

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

;; Configure elisp

;;; Code:

(require 'utils-common)
(require 'config-modegroups)

(cb:install-package 'elisp-slime-nav)
(cb:install-package 'cl-lib-highlight)

(custom-set-variables
 '(flycheck-emacs-lisp-load-path (list cb:lib-dir "./")))

(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Carton" . emacs-lisp-mode))

(add-hook 'cb:elisp-modes-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode 'cl-lib-highlight-initialize)
(add-hook 'emacs-lisp-mode 'cl-lib-highlight-warn-cl-initialize)

(hook-fn 'elisp-slime-nav-mode-hook
  (diminish 'elisp-slime-nav-mode))

;;; Flycheck

(defun cb:special-elisp-buffer? ()
  (and (derived-mode-p 'emacs-lisp-mode)
       (or
        (true? scratch-buffer)
        (s-ends-with? "-steps.el" (buffer-name))
        (s-matches? (rx bol (? (any "*" "."))
                        (or "org-"
                            "Org "
                            "Cask"
                            "Carton"
                            "scratch"
                            "emacs-lisp"
                            "autoloads"
                            (group "-pkg.el")
                            (group "Pp" (* anything) "Output")
                            "dir-locals"))
                    (buffer-name)))))

(defun cbel:configure-flycheck ()
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or (cb:special-elisp-buffer?) (buffer-narrowed-p)))
    (ignore-errors
      (flycheck-select-checker 'emacs-lisp))))

(add-hook 'flycheck-mode-hook 'cbel:configure-flycheck)

;;; Use paredit for `eval-expression'.

(hook-fn 'minibuffer-setup-hook
  (when (equal this-command 'eval-expression)
    (paredit-mode +1)))

;;; Snippet utils

(defun cbel:find-identifier-prefix ()
  "Find the commonest identifier prefix in use in this buffer."
  (let ((ns-separators (rx (or ":" "--" "/"))))
    (->> (buffer-string)
      ;; Extract the identifiers from declarations.
      (s-match-strings-all
       (rx bol (* space)
           "(" (? "cl-") (or "defun" "defmacro" "defvar" "defconst")
           (+ space)
           (group (+ (not space)))))
      ;; Find the commonest prefix.
      (-map 'cadr)
      (-filter (~ s-matches? ns-separators))
      (-map (C car (~ s-match (rx (group (* nonl) (or ":" "--" "/"))))))
      (-group-by 'identity)
      (-max-by (-on '>= 'length))
      (car))))

(defun cbel:find-group-for-snippet ()
  "Find the first group defined in the current file,
falling back to the file name sans extension."
  (or
   (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not
                                                       space))))
                  (buffer-string)))
   (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                  (buffer-string)))
   (f-no-ext (f-filename buffer-file-name))))

(defun cbel:simplify-arglist (text)
  "Return a simplified docstring of arglist TEXT."
  (->> (ignore-errors
         (read (format "(%s)" text)))
    (--keep
     (ignore-errors
       (cond
        ((listp it)
         (-first (-andfn 'symbolp (C (N (~ s-starts-with? "&")) symbol-name))
                 it))
        ((symbolp it) it))))
    (-remove (C (~ s-starts-with? "&") symbol-name))))

(defun cbel:cl-arglist? (text)
  "Non-nil if TEXT is a Common Lisp arglist."
  (let ((al (ignore-errors (read (format "(%s)" text)))))
    (or (-any? 'listp al)
        (-intersection al '(&key &allow-other-keys &body)))))

(defun cbel:defun-form-for-arglist (text)
  "Return either 'defun or 'cl-defun depending on whether TEXT
is a Common Lisp arglist."
  (if (cbel:cl-arglist? text) 'cl-defun 'defun))

(defun cbel:defmacro-form-for-arglist (text)
  "Return either 'defmacro or 'cl-defmacro depending on whether TEXT
is a Common Lisp arglist."
  (if (cbel:cl-arglist? text) 'cl-defmacro 'defmacro))

(defun cbel:process-docstring (text)
  "Format a function docstring for a snippet.
TEXT is the content of the docstring."
  (let ((docs (->> (cbel:simplify-arglist text)
                (-map (C s-upcase symbol-name))
                (s-join "\n\n"))))
    (unless (s-blank? docs)
      (concat "\n\n" docs))))

;;; IELM utils

(defun switch-to-ielm ()
  "Start up or switch to an Inferior Emacs Lisp buffer."
  (interactive)
  ;; HACK: rebind switch-to-buffer so ielm opens in another window.
  (noflet ((switch-to-buffer (buf) (switch-to-buffer-other-window buf)))
    (ielm)
    (cb:append-buffer)))

(defun switch-to-elisp ()
  "Switch to the last active elisp buffer."
  (interactive)
  (-when-let (buf (--first-buffer (derived-mode-p 'emacs-lisp-mode)))
    (switch-to-buffer-other-window buf)))

(defun send-to-ielm ()
  "Send the sexp at point to IELM."
  (interactive)
  (sp-kill-sexp nil 'yank)
  (unwind-protect
      (progn (switch-to-ielm)
             (delete-region (save-excursion
                              (search-backward-regexp (rx bol "ELISP>"))
                              (search-forward "> ")
                              (point))
                            (line-end-position))
             (yank))
    (setq kill-ring (cdr kill-ring))))

(defun eval-in-ielm ()
  "Eval the sexp at point in ielm."
  (interactive)
  (send-to-ielm)
  (ielm-return)
  (recenter -1)
  (switch-to-elisp))

;;; Hideshow

(after 'hideshow
  (add-to-list 'hs-special-modes-alist
               '(inferior-emacs-lisp-mode "(" ")" ";.*$" nil nil)))

(add-hook 'ielm-mode-hook 'hs-minor-mode)

(put 'ielm-mode 'comment-start ";")

;;; Define intelligent M-RET command.

(defconst cb-el:let-expression-re
  (regexp-opt '("(let" "(-if-let*" "(-when-let*"))
  "Regex matching the start of a let expression.")

(defun cb-el:let-expr-start ()
  "Move to the start of a let expression."
  (cl-flet ((at-let? () (thing-at-point-looking-at cb-el:let-expression-re)))
    (while (and (sp-backward-up-sexp) (not (at-let?))))
    (when (at-let?) (point))))

(defun cb-el:at-let-binding-form? ()
  "Non-nil if point is at the top of a binding form in a let expression."
  (and (save-excursion (cb-el:let-expr-start))
       (save-excursion
         (sp-backward-up-sexp 3)
         (thing-at-point-looking-at cb-el:let-expression-re))))

(defun cb-el:M-RET ()
  "Perform context-sensitive newline behaviour."
  (interactive)
  (cond
   ;; Insert let-binding
   ((save-excursion (cb-el:let-expr-start))
    (until (cb-el:at-let-binding-form?) (sp-backward-up-sexp))
    (sp-up-sexp)
    (newline-and-indent)
    (insert "()")
    (forward-char -1))
   (t
    (sp-up-sexp)
    (newline-and-indent)
    (when (true? evil-mode)
      (evil-insert-state)))))

;;; Advice

(defadvice eval-region (after region-evaluated-message activate)
  "Print feedback."
  (when (called-interactively-p nil)
    (message "Region evaluated.")))

(defadvice eval-buffer (after buffer-evaluated-feedback activate)
  "Print feedback."
  (when (called-interactively-p nil)
    (message "Buffer evaluated.")))

(hook-fn 'emacs-lisp-mode-hook
  (when (cb:special-elisp-buffer?) (setq-local no-byte-compile t))
  (add-hook 'after-save-hook 'check-parens nil t))

;;; Font-lock

(dash-enable-font-lock)

(font-lock-add-keywords
 'emacs-lisp-mode
 `(
   ;; Cl keywords
   (,(rx "(" (group (or "cl-destructuring-bind"
                        "cl-case")
                    symbol-end))
    (1 font-lock-keyword-face))

   ;; Macros and functions
   (,(rx bol (* space) "("
         (group-n 1 (or "cl-defun" "cl-defmacro"
                        "cl-defstruct"
                        "cl-defsubst"
                        "cl-deftype"))
         (+ space)
         (? "(")
         (group-n 2 (+? anything) symbol-end))
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))

   ;; General keywords
   (,(rx "(" (group (or "until"
                        "hook-fn"
                        "hook-fns"
                        "lambda+"
                        "after"
                        "noflet"
                        "ert-deftest"
                        "ac-define-source"
                        "evil-global-set-keys"
                        "flycheck-declare-checker"
                        "flycheck-define-checker")
                    symbol-end))
    (1 font-lock-keyword-face))

   ;; definition forms
   (,(rx bol (* space) "("
         (group-n 1
                  symbol-start
                  (* (not (any "(" space)))
                  (or "declare" "define" "extend" "gentest")
                  (+ (not space))
                  symbol-end)
         (+ space)
         (group-n 2 (+ (regex "\[^ )\n\]"))
                  symbol-end))
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face)))
 )

;;; Key bindings

(define-key emacs-lisp-mode-map (kbd "M-.")     'elisp-slime-nav-find-elisp-thing-at-point)
(define-key emacs-lisp-mode-map (kbd "C-c C-t") 'ert)
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'switch-to-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'send-to-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c RET") 'eval-in-ielm)
(define-key emacs-lisp-mode-map (kbd "M-RET")   'cb-el:M-RET)
(define-key emacs-lisp-mode-map (kbd "C-c C-f") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)

(after 'ielm
  (define-key emacs-lisp-mode-map (kbd "M-RET") 'newline-and-indent)
  (define-key emacs-lisp-mode-map (kbd "C-j") 'newline-and-indent)
  (define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'switch-to-elisp)
  )

(provide 'config-elisp)

;;; config-elisp.el ends here
