;;; cb-ido.el --- Configuration for ido

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0022

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

;; Configuration for ido

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'noflet)

(use-package ido
  :ensure t
  :idle   (require 'ido)
  :commands
  (ido-mode
   ido-find-file
   ido-switch-buffer
   ido-switch-buffer-other-window
   ido-display-buffer
   ido-kill-buffer
   ido-insert-buffer
   ido-switch-buffer-other-frame
   ido-find-file-in-dir
   ido-find-file-other-window
   ido-find-alternate-file
   ido-find-file-read-only
   ido-find-file-read-only-other-window
   ido-find-file-read-only-other-frame
   ido-display-file
   ido-find-file-other-frame
   ido-write-file
   ido-insert-file
   ido-dired
   ido-read-buffer
   ido-read-file-name
   ido-read-directory-name
   ido-completing-read)
  :init
  (progn
    (bind-keys
      "C-x C-f" 'ido-find-file
      "C-x d"   'ido-dired
      "C-x i"   'ido-insert-file
      "C-x C-w" 'ido-write-file
      "C-x k"   'ido-kill-buffer
      "C-x b"   'ido-switch-buffer))
  :config
  (progn
    (setq
     ido-enable-prefix            nil
     ido-save-directory-list-file (concat cb:tmp-dir "ido.last")
     ido-enable-flex-matching     t
     ido-create-new-buffer        'always
     ido-use-filename-at-point    'guess
     ido-max-prospects            10
     ido-default-file-method      'selected-window)
    (add-to-list 'ido-ignore-buffers "\\*helm.*")
    (add-to-list 'ido-ignore-buffers "\\*Minibuf.*")
    (add-to-list 'ido-ignore-files "\\.swp")
    (add-to-list 'ido-ignore-files "\\.DS_Store")

    (hook-fn 'ido-setup-hook
      ;; Typing ~ resets ido prompt to home directory.
      (define-key ido-common-completion-map
        (kbd "~")
        (command
         (if (looking-back "/")
             (insert "~/")
           (call-interactively 'self-insert-command)))))

    (ido-mode +1)))

(use-package ido-hacks
  :ensure t
  :commands ido-hacks-mode
  :init (after 'ido (ido-hacks-mode +1)))

(use-package ido-ubiquitous
  :ensure t
  :disabled t
  :commands ido-ubiquitous-mode
  :init
  (after 'ido
    (macrolet
        ((use-new-completing-read
          (cmd package)
          `(eval-after-load ,package
             '(defadvice ,cmd (around ido-ubiquitous-new activate)
                (let ((ido-ubiquitous-enable-compatibility nil))
                  ad-do-it)))))

      (use-new-completing-read yas/expand 'yasnippet)
      (use-new-completing-read yas/visit-snippet-file 'yasnippet))

    (ido-mode +1)
    (ido-ubiquitous-mode +1)))

(use-package ido-yes-or-no
  :disabled t
  :ensure t
  :commands ido-yes-or-no-mode
  :init (after 'ido (ido-yes-or-no-mode +1)))

(use-package flx
  :ensure t
  :defer t)

(use-package flx-ido
  :ensure t
  :config
  (after 'ido
    (flx-ido-mode +1)
    ;; Override ido faces with flx ones.
    (setq ido-use-faces nil)))

(use-package imenu
  :commands imenu
  :init
  (hook-fn 'emacs-lisp-mode-hook
    "Display section headings."
    (setq imenu-prev-index-position-function nil)
    (add-to-list 'imenu-generic-expression
                 `("SECTION"
                   ;; Match sections with at least 3 semicolons
                   ,(rx bol (* space) ";;;" (* ";") (+ space) (group (+ nonl )))
                   1) t)))

(provide 'cb-ido)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-ido.el ends here
