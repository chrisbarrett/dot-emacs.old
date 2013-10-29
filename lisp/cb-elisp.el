;;; cb-elisp.el --- Configuration for elisp

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0003

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

;; Configuration for elisp

;;; Code:

(require 'use-package)
(require 'noflet)
(require 'cb-lib)
(require 'cb-evil)
(require 'cb-search)
(autoload 'sp-kill-sexp "smartparens")

(defun cb:special-elisp-buffer? ()
  (and (derived-mode-p 'emacs-lisp-mode)
       (or
        (true? scratch-buffer)
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

;; Prevent flycheck from running checkdoc for certain elisp file types.
(after 'flycheck
  (hook-fn 'flycheck-mode-hook
    (when (cb:special-elisp-buffer?)
      (flycheck-select-checker 'emacs-lisp)))

  (setq-default flycheck-emacs-lisp-load-path (list cb:lib-dir "./")))

;; Add command to switch to corresponding unit test.
(after 'projectile

  (defun src<->code ()
    "Switch between a source file and its corresponding test."
    (interactive)
    (find-file
     (let ((fname (file-name-nondirectory (buffer-file-name))))
       ;; If it contains `test`, find a source file at the project root.
       (if (s-contains? "tests" fname)
           (concat (projectile-project-root) (s-replace "-tests" "" fname))

         ;; Otherwise it's a source file, find a corresponding test.
         (concat (projectile-project-root) "test/"
                 (cl-destructuring-bind (_ name ext)
                     (s-match (rx bol (group (* nonl)) (group "." (* alnum) eol))
                              fname)
                   (concat name "-tests" ext)))))))

  (define-key emacs-lisp-mode-map (kbd "C-c C-j") 'src<->code))

;; Customise font-locking for elisp.
(after 'lisp-mode
  (--each cb:elisp-modes
    (font-lock-add-keywords
     it
     `(
       ;; General keywords
       (,(rx "(" (group (or "use-package"
                            "configuration-group"
                            "until"
                            "cal"
                            "hook-fn"
                            "hook-fns"
                            "lambda+"
                            "after"
                            "noflet"
                            "ac-define-source"
                            "evil-global-set-keys"
                            "flycheck-declare-checker"
                            "flycheck-define-checker")
                        symbol-end)
             word-end)
        (1 font-lock-keyword-face))

       ;; Identifiers after keywords
       (,(rx "(" (group (or "use-package"
                            "ac-define-source"
                            "flycheck-declare-checker"
                            "flycheck-define-checker"))
             (+ space)
             (group (+ (regex "\[^ )\n\]"))
                    symbol-end))
        (2 font-lock-constant-face))

       ;; definition forms
       (,(rx bol (* space) "("
             (group-n 1
                      symbol-start
                      (* (not space))
                      (or "declare" "define" "extend" "gentest")
                      (+ (not space))
                      symbol-end)
             (+ space)
             (group-n 2 (+ (regex "\[^ )\n\]"))
                      symbol-end))
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face))))))

;; Configure sp for IELM.
(after 'smartparens
  (add-hook 'ielm-mode-hook 'smartparens-strict-mode))

;; Configure auto-complete for IELM.
(after 'auto-complete
  (add-to-list 'ac-modes 'ielm-mode)
  (hook-fn 'ielm-mode-hook
    (auto-complete-mode +1)
    (setq ac-sources '(ac-source-features
                       ac-source-functions
                       ac-source-yasnippet
                       ac-source-variables
                       ac-source-symbols))))

;; Configure hideshow for IELM.
(hook-fn 'ielm-mode-hook
  (with-current-buffer "*ielm*"
    (message "Setting commnent vars")
    (setq-local comment-start ";")
    (setq-local comment-end "")
    (hs-minor-mode +1)))

(after 'hideshow
  (add-to-list 'hs-special-modes-alist
               '(inferior-emacs-lisp-mode "(" ")" ";.*$" nil nil)))

;; Add evil documentation lookup for elisp.
(after 'cb-evil

  (defun get-elisp-doc (sym)
    "Find the appropriate documentation for SYM."
    (when (apply 'derived-mode-p cb:elisp-modes)
      (cond
       ((symbol-function sym)
        (describe-function sym))
       ((and (boundp sym) (not (facep sym)))
        (describe-variable sym))
       ((facep sym)
        (describe-face sym))
       (t
        (user-error "No documentation available")))
      major-mode))

  (add-hook 'evil-find-doc-hook (C get-elisp-doc intern)))

;; Add elisp functions to global search picker.
(after 'cb-search

  (cbs-define-search-method
   :name "Apropos"
   :key "a"
   :command
   (lambda (_)
     (call-interactively 'helm-apropos))
   :when
   (lambda ()
     (apply 'derived-mode-p cb:elisp-modes)))

  (cbs-define-search-method
   :name "Lisp Function"
   :key "f"
   :command
   (lambda (_)
     (call-interactively 'find-function))
   :when
   (lambda ()
     (apply 'derived-mode-p cb:elisp-modes)))

  (cbs-define-search-method
   :name "Lisp Library"
   :key "l"
   :command
   (lambda (_)
     (call-interactively 'find-library))
   :when
   (lambda ()
     (apply 'derived-mode-p cb:elisp-modes)))

  (cbs-define-search-method
   :name "Lisp Variable"
   :key "v"
   :command
   (lambda (_)
     (call-interactively 'find-variable))
   :when
   (lambda ()
     (apply 'derived-mode-p cb:elisp-modes))))

(hook-fn 'minibuffer-setup-hook
  "Enable Paredit during eval-expression."
  (when (equal this-command 'eval-expression)
    (paredit-mode +1)))

(use-package lisp-mode
  :defer t
  :mode  (("Carton" . emacs-lisp-mode)
          ("Cask" . emacs-lisp-mode))
  :config
  (progn

    (define-keys emacs-lisp-mode-map
      "C-c C-t" 'ert
      "C-c e b" 'eval-buffer
      "C-c C-l" 'emacs-lisp-byte-compile-and-load
      "C-c C-z" 'switch-to-ielm
      "C-c e r" 'eval-region)

    ;;;; IELM

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
      "Send the sexp at point to IELM"
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

    (hook-fn 'ielm-mode-hook
      (local-set-key (kbd "C-c C-z") 'switch-to-elisp))

    (define-keys emacs-lisp-mode-map
      "C-c C-c" 'send-to-ielm
      "C-c RET" 'eval-in-ielm)


    ;;;; File handling

    (hook-fn 'emacs-lisp-mode-hook
      (when (cb:special-elisp-buffer?)
        (setq-local no-byte-compile t))
      ;; Check parens are balanced and byte-compile.
      (hook-fn 'after-save-hook
        :local t
        (check-parens)
        (byte-compile-file (buffer-file-name))))

    ;;;; Advices

    (defadvice eval-buffer (after buffer-evaluated-feedback activate)
      "Message that the buffer has been evaluated."
      (when (buffer-file-name)
        (message "Buffer evaluated.")))))

(use-package cl-lib-highlight
  :ensure t
  :init (after 'lisp-mode
          (cl-lib-highlight-initialize)
          (cl-lib-highlight-warn-cl-initialize)))

(use-package edebug
  :defer t
  :commands edebug-next-mode
  :init
  (hook-fn 'emacs-lisp-mode-hook
    (local-set-key (kbd "C-x X d") 'edebug-defun)))

(use-package redshank
  :ensure   t
  :commands turn-on-redshank-mode
  :diminish redshank-mode
  :init     (add-hook 'cb:lisp-modes-hook 'turn-on-redshank-mode))

(use-package macrostep
  :ensure t
  :bind   ("C-c e m" . macrostep-expand)
  :config
  (after 'evil
    (evil-add-hjkl-bindings macrostep-mode-map 'motion)))

(use-package elisp-slime-nav
  :ensure   t
  :diminish elisp-slime-nav-mode
  :commands elisp-slime-nav-mode
  :defer    t
  :init
  (hook-fn 'cb:elisp-modes-hook
    (elisp-slime-nav-mode +1)
    (local-set-key (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

    ;; Make M-. work in normal state.
    (after 'evil
      (evil-local-set-key 'normal (kbd "M-.")
                          'elisp-slime-nav-find-elisp-thing-at-point))))

(use-package litable
  :ensure   t
  :commands litable-mode)

(provide 'cb-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-elisp.el ends here
