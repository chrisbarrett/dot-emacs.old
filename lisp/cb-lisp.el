;;; cb-lisp.el --- Configuration for all lisp modes

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0005

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

;; Configuration for all lisp modes

;;; Code:

(require 'use-package)
(require 'cb-foundation)

(hook-fn 'cb:lisp-modes-hook
  (local-set-key (kbd "M-q") 'indent-dwim))

(after 'smartparens

  (--each cb:lisp-modes
    (add-to-list 'sp-navigate-reindent-after-up it))

  (sp-with-modes cb:lisp-modes
    (sp-local-pair "`" "`" :when '(sp-in-string-p))
    (sp-local-pair "'" "'" :when '(sp-in-string-p))))

(use-package parenface-plus
  :ensure t
  :defer  t
  :idle   (require 'parenface-plus)
  :init
  (hook-fn 'prog-mode-hook (require 'parenface-plus)))

(use-package eval-sexp-fu
  :commands eval-sexp-fu-flash-mode
  :init     (add-hook 'cb:lisp-modes-hook 'eval-sexp-fu-flash-mode)
  :config   (setq eval-sexp-fu-flash-duration 0.2))

(use-package eldoc
  :commands eldoc-mode
  :diminish eldoc-mode
  :init
  (add-hook 'cb:lisp-modes-hook 'turn-on-eldoc-mode))

(use-package slime
  :defer    t
  :commands (slime-setup slime)
  :init
  (defun run-slime ()
    "Run slime, prompting for a lisp implementation."
    (interactive)
    (let ((current-prefix-arg '-))
      (slime)))
  :config
  (progn
    (setq slime-lisp-implementations `((lisp ("sbcl" "--noinform"))))
    (slime-setup '(slime-fancy))))

(use-package ac-slime
  :ensure   t
  :defer    t
  :commands (set-up-slime-ac)
  :init     (add-hook 'slime-modes-hook 'set-up-slime-ac)
  :config   (add-to-list 'ac-modes 'slime-repl-mode))

(provide 'cb-lisp)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-lisp.el ends here
