;;; cb-language-utils.el --- Utilities useful for all languages

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0012

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

;; Utilities useful for all languages

;;; Code:

(require 'use-package)

(use-package emr
  :ensure t
  :bind   ("M-RET" . emr-show-refactor-menu)
  :config (emr-initialize))

(use-package paredit
  :ensure t
  :idle   (require 'paredit)
  :diminish paredit-mode
  :commands
  (paredit-mode
   enable-paredit-mode
   disable-paredit-mode)
  :init
  (progn

    (hook-fn 'minibuffer-setup-hook
      "Use paredit in the minibuffer."
      (when (eq this-command 'eval-expression)
        (paredit-mode t)))

    (hook-fn 'paredit-mode-hook
      "Turn off smart parens."
      (when (featurep 'smartparens)
        (turn-off-smartparens-mode))))

  :config
  (progn
    (use-package cb-paredit)

    (defun cb:paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))

    (define-key paredit-mode-map (kbd "M-)")
      'cb:paredit-wrap-round-from-behind)

    (define-key paredit-mode-map (kbd "M-[")
      'paredit-wrap-square)

    (define-key paredit-mode-map (kbd "M-{")
      'paredit-wrap-curly)

    (define-key paredit-mode-map (kbd "M-r") nil)

    (add-hook 'cb:lisp-modes-hook 'enable-paredit-mode)))

(use-package smartparens
  :ensure t
  :idle   (require 'smartparens)
  :diminish smartparens-mode
  :commands
  (smartparens-mode
   smartparens-global-mode)
  :init
  (progn
    (add-hook 'text-mode-hook 'turn-on-smartparens-mode)
    (hook-fn 'comint-mode-hook (smartparens-mode +1))

    (hook-fn 'prog-mode-hook
      "Ensure Paredit is used for Lisps."
      (if (-contains? cb:lisp-modes major-mode)
          (paredit-mode +1)
        (smartparens-mode +1))))
  :config
  (progn
    ;; Customise to behave more like paredit.
    (setq sp-navigate-close-if-unbalanced t)
    (define-key smartparens-mode-map (kbd "DEL")    'sp-backward-delete-char)
    (define-key smartparens-mode-map (kbd "M-DEL")  'sp-backward-kill-symbol)
    (define-key smartparens-mode-map (kbd "C-k")    'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd ")") 'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "]") 'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "}") 'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "M-<up>")
      (command (if (derived-mode-p 'org-mode)
                   (org-metaup)
                 (sp-splice-sexp-killing-backward))))
    (define-key smartparens-mode-map (kbd "M-<down>")
      (command (if (derived-mode-p 'org-mode)
                   (org-metadown)
                 (sp-splice-sexp-killing-forward))))

    (sp-pair "'" nil :unless '(sp-point-after-word-p))
    (sp-local-tag '(sgml-mode html-mode) "<" "<_>" "</_>"
                  :transform 'sp-match-sgml-tags)))

(use-package smart-operator
  :ensure t
  :idle   (require 'smart-operator)
  :commands
  (smart-insert-operator
   smart-insert-operator-hook)
  :init
  (progn

    (defmacro smart-op (op)
      "Make a smart operator command that will insert OP."
      `(command (smart-insert-operator ,op)))

    (defun cb:python-equals ()
      "Insert an '=' char padded by spaces, except in function arglists."
      (interactive)
      (if (string-match-p (rx (* space) "def ")
                          (thing-at-point 'line))
          (insert "=")
        (smart-insert-operator "=")))

    (hook-fn 'cb:python-modes-hook
      (smart-insert-operator-hook)
      (local-set-key (kbd "=") 'cb:python-equals)
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'cb:ruby-modes-hook
      (smart-insert-operator-hook)
      (local-set-key (kbd "=") 'cb:python-equals)
      (local-set-key (kbd "~") (smart-op "~"))
      (local-unset-key (kbd "%"))
      (local-unset-key (kbd "&"))
      (local-unset-key (kbd "/"))
      (local-unset-key (kbd "."))
      (local-unset-key (kbd ":")))

    (hook-fn 'cb:markup-modes-hook
      (local-set-key (kbd ",") (smart-op ",")))

    (hook-fn 'cb:haskell-modes-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd ":"))
      (local-unset-key (kbd ".")))

    (hook-fn 'sclang-mode-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd "|"))
      (local-unset-key (kbd ".")))

    (hook-fn 'asm-mode-hook
      (smart-insert-operator-hook)
      (local-unset-key (kbd "%"))
      (local-unset-key (kbd "-"))
      (local-unset-key (kbd "."))))

  :config
  (defadvice smart-insert-operator (around normal-insertion-for-string activate)
    "Do not perform smart insertion if looking at a string."
    (if (-contains? '(font-lock-string-face
                      font-lock-doc-face
                      font-lock-doc-string-face
                      font-lock-comment-face)
                    (face-at-point))
        (insert (ad-get-arg 0))
      (prog1 ad-do-it
        (when (equal (ad-get-arg 0) "=")
          (save-excursion
           (indent-according-to-mode)))))))

(use-package lambda-mode
  :defer t
  :idle  (require 'lambda-mode)
  :diminish lambda-mode
  :commands lambda-mode
  :init
  (progn
    (setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
    (add-hook 'cb:scheme-modes-hook    'lambda-mode)
    (add-hook 'inferior-lisp-mode-hook 'lambda-mode)
    (add-hook 'lisp-mode-hook          'lambda-mode)
    (add-hook 'cb:elisp-modes-hook     'lambda-mode)
    (add-hook 'cb:python-modes-hook    'lambda-mode)
    (add-hook 'cb:slime-modes-hook     'lambda-mode)))

(use-package paren
  :defer  t
  :idle   (require 'paren)
  :init   (hook-fn 'prog-mode-hook (require 'paren))
  :config (show-paren-mode +1))

(use-package highlight-parentheses
  :ensure t
  :defer  t
  :commands highlight-parentheses-mode
  :diminish highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

(use-package highlight-symbol
  :ensure   t
  :diminish highlight-symbol-mode
  :commands highlight-symbol-mode
  :init     (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :config   (setq highlight-symbol-idle-delay 0.5))

(provide 'cb-language-utils)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-language-utils.el ends here
