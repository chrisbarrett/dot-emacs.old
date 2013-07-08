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
  :bind ("M-RET" . emr-show-refactor-menu)
  :init (add-hook 'prog-mode-hook 'emr-initialize))

(use-package smart-operator
  :ensure t
  :init
  (defmacro smart-op (op)
    "Make a smart operator command that will insert OP."
    `(command (smart-insert-operator ,op)))
  :config
  (progn

    (defadvice smart-insert-operator (after indent-after-insert-equals activate)
      "Reindent the current line after inserting an equals."
      (when (equal (ad-get-arg 0) "=")
        (save-excursion
          (indent-according-to-mode))))

    (defadvice smart-insert-operator (around normal-insertion-for-string activate)
      "Use self-insert rather than smart operator when looking at string or comment."
      (if (or
           ;; Looking at a string?
           (-contains? '(font-lock-string-face
                         font-lock-doc-face
                         font-lock-doc-string-face
                         font-lock-comment-face)
                       (face-at-point))
           ;; Looking at quotation mark?
           (-contains? '(?\" ?\') (char-after)))
          (insert (ad-get-arg 0))
        ad-do-it))))

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
