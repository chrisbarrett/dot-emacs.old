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

;; `undo-tree' provides a graphical view of the undo history.
(use-package undo-tree
  :ensure   t
  :idle     (require 'undo-tree)
  :bind     ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :init     (hook-fn 'find-file-hook (require 'undo-tree))
  :config (global-undo-tree-mode +1))

;; `scratch' lets you quickly create scratch buffers.
(use-package scratch
  :ensure   t
  :commands scratch
  :bind     ("C-c e s" . scratch))

;; `iedit' provides commands for transforming all occurrences of a symbol in the buffer.
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

    (defun cbiedit:replace-read ()
      (iedit-replace-occurrences (read-string "Replace in buffer: ")))

    (defun cbiedit:restrict-to-region ()
      (iedit-restrict-region (region-beginning) (region-end)))

    (defun cbiedit:remove-region ()
      (iedit-restrict-region (region-beginning) (region-end) t))

    (defun cbiedit:replace-in-region ()
      (cbiedit:restrict-to-region)
      (cbiedit:replace-read))

    (define-command-picker iedit-picker
      :title "*iedit*"
      :options
      '(("e" "Expand"              iedit-expand-by-a-line         :unless region-active-p)
        ("p" "Expand (up)"         iedit-expand-up-a-line         :unless region-active-p)
        ("n" "Expand (down)"       iedit-expand-down-a-line       :unless region-active-p)
        ("r" "Replace (in region)" cbiedit:replace-in-region      :when region-active-p)
        ("r" "Replace"             cbiedit:replace-read           :unless region-active-p)
        ("k" "Delete Matches"      iedit-delete-occurrences       :unless region-active-p)
        ("l" "Restrict (line)"     iedit-restrict-current-line    :unless region-active-p)
        ("R" "Restrict (region)"   cbiedit:restrict-to-region     :when region-active-p)

        ("x" "Remove (region)"     cbiedit:remove-region          :when region-active-p)
        ("f" "Restrict (function)" iedit-restrict-function        :when (lambda () (thing-at-point 'defun)))
        ("c" "Toggle Case-Sensitivity" iedit-toggle-case-sensitive)
        ("t" "Toggle at Point"     iedit-toggle-selection)
        ("d" "Done"                iedit-done)))

    (bind-key "C-<return>" 'iedit-picker iedit-mode-keymap)))

;; `info-lookmore' adds support for searching the CL Hyperspec.
(use-package info-lookmore
  :commands info-lookmore-elisp-cl
  :init     (after 'info-look (info-lookmore-elisp-cl)))

;; `proced' provides a UI for managing system processes.
(use-package proced
  :defer t
  :bind ("C-x p" . proced))

;; `ack-and-a-half' provides an Elisp interface to the ack search utility.
(use-package ack-and-a-half
  :ensure t
  :commands
  (ack-and-a-half-same
   ack-and-a-half-find-file
   ack-and-a-half-find-file-same))

;; `smooth-scrolling' changes the default scrolling behaviour to keep point away
;; from the top or bottom of the window in order to show scrolling context.
(use-package smooth-scrolling
  :ensure t)

;; `midnight' runs a hook at midnight. By default it will clean unused buffers.
(use-package midnight
  :ensure t
  :defer  t
  :idle (require 'midnight))

;; `ace-jump-mode' provides commands for quickly moving around the visible part
;; of the buffer.
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

;; `hideshow' provides code folding.
(use-package hideshow
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :defer    t)

;; `abbrev' lets you declare abbreviations that will be automatically expanded
;; when typed.
(use-package abbrev
  :defer t
  :config
  (setq abbrev-file-name (concat cb:tmp-dir "abbrev_defs")))

;; `multiple-cursors' lets you perform editing commands on different lines
;; simultaneously.
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

;; `dictionary' provides dictionary search functions.
(use-package dictionary
  :ensure t
  :commands
  (dictionary
   dictionary-search))

;; `autorevert' is a minor mode that reverts the buffer if it changes on disk.
(use-package autorevert
  :commands (auto-revert-mode
             turn-on-auto-revert-mode
             auto-revert-tail-mode
             turn-on-auto-revert-tail-mode
             global-auto-revert-mode)
  :diminish auto-revert-mode)

;; `imenu' provides navigation between sections of the current buffer.
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

;; `file-template' provides file skeletons.
(use-package file-template
  :if (not noninteractive)
  :config
  (progn
    (autoload 'file-template-find-file-not-found-hook "file-template" nil t)
    (add-hook 'find-file-not-found-hooks 'file-template-find-file-not-found-hook 'append)

    (defvar cb:file-templates-dir (f-join user-emacs-directory "templates"))

    (setq file-template-insert-automatically t
          file-template-paths (list cb:file-templates-dir))

    ;; Start the undo history after the expansion is complete.
    (hook-fn 'file-template-insert-hook
      (setq buffer-undo-list nil
            buffer-undo-tree nil))

    (setq file-template-mapping-alist
          (->> (f-files cb:file-templates-dir)
            (-map 'f-filename)
            (--map (cons (format "\\.%s$" (f-ext it)) it))))))

;; `expand-region' provides commands for making smart selections.
(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-<up>") 'er/expand-region)
    (global-set-key (kbd "M-<down>") 'er/contract-region)))

(provide 'cb-productivity)

;; Local Variables:
;; End:

;;; cb-productivity.el ends here
