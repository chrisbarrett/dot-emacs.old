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

(font-lock-add-keywords
 'emacs-lisp-mode
 `(
   ;; General keywords
   (,(rx "(" (group (or "use-package"
                        "hook-fn"
                        "after"
                        "ac-define-source"
                        "flycheck-declare-checker"
                        "cl-destructuring-bind"
                        "cl-defstruct"))
         word-end)
    (1 font-lock-keyword-face))

   ;; Identifiers after keywords
   (,(rx "(" (group (or "use-package"
                        "ac-define-source"
                        "flycheck-declare-checker"))
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))
    (2 font-lock-constant-face))

   ;; definition forms
   (,(rx "("
         (group-n 1
                  (* (not space))
                  (or "declare" "define")
                  (+ (not space)))
         (+ space)
         (group-n 2 (+ (regex "\[^ )\n\]"))))
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))

   ;; cb:extracting-list
   (,(rx "(" (group "cb:extracting-list") (or space eol))
    (1 font-lock-keyword-face))

   ;; cl-struct.
   (,(rx "(cl-defstruct"
         (+ space)
         (group (+ (regex "\[^ )\n\]"))))

    (1 font-lock-type-face))))

(use-package lisp-mode
  :defer t
  :mode  (("Carton" . emacs-lisp-mode))
  :config
  (progn

    (defun cb:switch-to-ielm ()
      "Start up or switch to an Inferior Emacs Lisp buffer."
      (interactive)
      ;; HACK: rebind switch-to-buffer so ielm opens in another window.
      (flet ((switch-to-buffer (buf) (switch-to-buffer-other-window buf)))
        (ielm)
        (cb:append-buffer)))

    (defun cb:last-elisp-buffer ()
      "Find the last active Elisp buffer."
      (--first (with-current-buffer it
                 (equal 'emacs-lisp-mode major-mode))
               (buffer-list)))

    (defun cb:switch-to-elisp ()
      "Switch to the last active elisp buffer."
      (interactive)
      (-when-let (buf (cb:last-elisp-buffer))
        (switch-to-buffer-other-window buf)))

    (define-key emacs-lisp-mode-map (kbd "C-c C-t") 'ert)
    (define-key emacs-lisp-mode-map (kbd "C-c e b") 'eval-buffer)
    (define-key emacs-lisp-mode-map (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'cb:switch-to-ielm)
    (define-key emacs-lisp-mode-map (kbd "C-c e r") 'eval-region)

    (hook-fn 'ielm-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-elisp))

    (defun cb:special-elisp-file? ()
      (and (derived-mode-p 'emacs-lisp-mode)
           (-contains? '("*scratch*" ".dir-locals.el")
                       (buffer-name))))

    (defun cb:elisp-after-save ()
      "Check parens are balanced and byte-compile."
      (check-parens)
      (ignore-errors
        (unless (or (cb:special-elisp-file?) no-byte-compile)
          (byte-compile-file (buffer-file-name)))))

    (hook-fn 'emacs-lisp-mode-hook
      (add-hook 'after-save-hook 'cb:elisp-after-save t 'local))

    (hook-fn 'flycheck-mode-hook
      "Disable flycheck mode for scratch buffer."
      (when (cb:special-elisp-file?)
        (add-hook 'flycheck-before-syntax-check-hook (lambda () (flycheck-mode -1)) nil t)))

    (defadvice eval-buffer (after buffer-evaluated-feedback activate)
      "Message that the buffer has been evaluated."
      (when (buffer-file-name)
        (message "Buffer evaluated.")))))

(use-package edebug
  :defer t
  :init
  (progn
    (autoload 'edebug-next-mode "edebug")
    (hook-fn 'emacs-lisp-mode-hook
      (local-set-key (kbd "C-x X d") 'edebug-defun))))

(use-package ert-modeline
  :defer    t
  ;; The status is added to the mode-line format directly.
  :diminish ert-modeline-mode
  :commands ert-modeline-mode
  :init     (add-hook 'emacs-lisp-mode-hook 'ert-modeline-mode))

(use-package redshank
  :ensure   t
  :commands turn-on-redshank-mode
  :diminish redshank-mode
  :init     (add-hook 'cb:lisp-modes-hook 'turn-on-redshank-mode))

(use-package macrostep
  :ensure t
  :bind   ("C-c e m" . macrostep-expand)
  :config (evil-add-hjkl-bindings macrostep-mode-map 'motion))

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
    (when (featurep 'evil)
      (evil-local-set-key 'normal (kbd "M-.")
                          'elisp-slime-nav-find-elisp-thing-at-point))))

(use-package litable
  :ensure   t
  :commands litable-mode
  :defer    t
  :init     (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'litable-mode))

(provide 'cb-elisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-elisp.el ends here
