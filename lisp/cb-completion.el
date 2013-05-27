;;; cb-completion.el --- Configuration for completion utilities

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0016

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

;; Configuration for completion utilities

;;; Code:

(require 'use-package)

(use-package auto-complete
  :ensure   t
  :idle     (require 'auto-complete)
  :diminish auto-complete-mode
  :commands
  (global-auto-complete-mode
   auto-complete-mode)

  :init
  (progn
    (after 'auto-complete (global-auto-complete-mode +1))
    (add-hook 'find-file-hook 'auto-complete-mode))

  :config
  (progn

    (use-package auto-complete-config
      :config (ac-config-default))

    (add-to-list 'ac-dictionary-directories
                 (concat user-emacs-directory "ac-dict"))

    (--each cb:lisp-modes (add-to-list 'ac-modes it))
    (setq
     ac-auto-show-menu t
     ac-dwim t
     ac-use-menu-map t
     ac-quick-help-delay 0.4
     ac-quick-help-height 60
     ac-disable-inline t
     ac-show-menu-immediately-on-auto-complete t
     ac-auto-start 2
     ac-candidate-menu-min 0
     ac-comphist-file (concat cb:tmp-dir "ac-comphist.dat"))

    (ac-flyspell-workaround)

    (define-key ac-completing-map (kbd "C-n") 'ac-next)
    (define-key ac-completing-map (kbd "C-p") 'ac-previous)
    (define-key ac-completing-map "\t" 'ac-complete)
    (define-key ac-completing-map (kbd "M-RET") 'ac-help)))

(use-package yasnippet
  :ensure t
  :idle   (require 'yasnippet)
  :diminish yas-minor-mode
  :commands
  (yas-global-mode
   yas-minor-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'sgml-mode-hook 'yas-minor-mode))
  :config
  (progn
    (setq
     yas-prompt-functions'(yas-ido-prompt)
     yas/trigger-key (kbd "RET"))
    (add-to-list 'yas-snippet-dirs cb:yasnippet-dir)
    (yas-global-mode t)
    (hook-fn 'snippet-mode-hook
      (setq require-final-newline nil))))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package abbrev
  :defer    t
  :diminish abbrev-mode)

(use-package fuzzy
  :ensure t)

(provide 'cb-completion)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-completion.el ends here
