;;; cb-productivity.el --- Miscellaneous productivity configuration

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0014

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

;; Miscellaneous productivity configuration

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-paths)

(use-package undo-tree
  :ensure   t
  :idle     (require 'undo-tree)
  :bind     ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :init     (hook-fn 'find-file-hook (require 'undo-tree))
  :config (global-undo-tree-mode +1))

(use-package scratch
  :ensure   t
  :commands scratch
  :bind     ("C-c e s" . scratch))

(use-package iedit
  :ensure   t
  :bind
  ("C-<return>" . iedit-mode)
  :commands
  (iedit-mode
   iedit-replace-occurrences
   iedit-done)
  :init
  (progn

    (defun rename-symbol-in-defun ()
      (interactive)
      (iedit-mode 0)
      (unwind-protect
          (iedit-replace-occurrences (read-string "Replace in function: "))
        (iedit-done)))

    (defun rename-symbol-in-buffer ()
      (interactive)
      (iedit-mode)
      (unwind-protect
          (iedit-replace-occurrences (read-string "Replace in buffer: "))
        (iedit-done)))

    (bind-keys
      "C-c r" 'iedit-mode
      "M-r" 'rename-symbol-in-defun
      "M-R" 'rename-symbol-in-buffer))

  :config
  (after 'iedit

    (defvar cbiedit:options
      '(("c" "Toggle Case-Sensitivity" iedit-toggle-case-sensitive)
        ("e" "Expand" iedit-expand-by-a-line)
        ("f" "Restrict (function)" iedit-restrict-function)
        ("l" "Restrict (line)" iedit-restrict-current-line)
        ("n" "Expand (down)" iedit-expand-down-a-line)
        ("p" "Expand (up)" iedit-expand-up-a-line)
        ("k" "Delete Matches" iedit-delete-occurrences)
        ("d" "Done" iedit-done)
        ("r" "Restrict (region)" iedit-restrict-region)
        ("t" "Toggle at Point" iedit-toggle-selection))
      "The list of options used for the iedit option picker.")

    (defun cbiedit:read-option ()
      "Read an iedit action interactively."
      (interactive)
      (cl-destructuring-bind (_ _ fn) (read-option "iedit" 'car 'cadr cbiedit:options)
        (funcall fn)))

    (bind-key "C-<return>" 'cbiedit:read-option iedit-mode-keymap)))

(use-package info-lookmore
  :commands info-lookmore-elisp-cl
  :init     (after 'info-look (info-lookmore-elisp-cl)))

(use-package proced
  :defer t
  :bind ("C-x p" . proced))

(use-package ack-and-a-half
  :ensure t
  :commands
  (ack-and-a-half-same
   ack-and-a-half-find-file
   ack-and-a-half-find-file-same))

(use-package smooth-scrolling
  :ensure t)

(use-package midnight
  :ensure t
  :defer  t
  :idle (require 'midnight))

(use-package ace-jump-mode
  :ensure t
  :bind ("S-<return>" . ace-jump-word-mode)
  :init
  (hook-fn 'evil-mode-hook
    (require 'ace-jump-mode))
  :config
  (progn
    (hook-fn 'ace-jump-mode-end-hook
      (ignore-errors (exit-recursive-edit)))
    ;; Use ESC to quit ace-jump.
    (hook-fns '(ace-jump-line-mode ace-jump-word-mode ace-jump-char-mode)
      (local-set-key (kbd "ESC") 'keyboard-quit))))

(use-package hideshow
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :defer    t)

(use-package abbrev
  :defer t
  :config
  (setq abbrev-file-name (concat cb:tmp-dir "abbrev_defs")))

(use-package multiple-cursors
  :ensure t
  :defer t
  :init
  (bind-keys
    :overriding? t
    "C-c m m" 'mc/edit-lines
    "C-c m a" 'mc/mark-all-dwim
    "C-c m n" 'mc/mark-next-like-this
    "C-c m p" 'mc/mark-previous-like-this)
  :config
  (setq mc/list-file (f-join cb:tmp-dir "multiple-cursors-list.el")))

(provide 'cb-productivity)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-productivity.el ends here
