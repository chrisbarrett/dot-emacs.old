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

(use-package undo-tree
  :ensure   t
  :idle     (require 'undo-tree)
  :bind     ("C-x u" . undo-tree-visualize)
  :diminish undo-tree-mode
  :init     (hook-fn 'find-file-hook (require 'undo-tree))
  :config   (global-undo-tree-mode +1))

(use-package key-chord
  :ensure t
  :defer  t
  :init
  (hook-fn 'after-init-hook (key-chord-mode +1))
  :config
  (progn
    (key-chord-define-global "x;" 'kill-current-buffer)
    (after 'smartparens
      (key-chord-define-global "qj" 'sp-backward-slurp-sexp)
      (key-chord-define-global "qk" 'sp-forward-slurp-sexp)
      (key-chord-define-global "ql" 'sp-splice-sexp-killing-backward)
      (key-chord-define-global "qn" 'sp-backward-barf-sexp)
      (key-chord-define-global "qm" 'sp-forward-barf-sexp))))

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
  (hook-fn 'after-init-hook

    (defun cb:rename-symbol-in-defun (replacement)
      (interactive "sReplace in function: ")
      (iedit-mode 0)
      (iedit-replace-occurrences replacement)
      (iedit-done))

    (defun cb:rename-symbol-in-buffer (replacement)
      (interactive "sReplace in buffer: ")
      (iedit-mode)
      (iedit-replace-occurrences replacement)
      (iedit-done))

    (bind-key "M-r" 'cb:rename-symbol-in-defun)
    (bind-key "M-R" 'cb:rename-symbol-in-buffer)))

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
  :bind
  (("C-L" . ace-jump-line-mode)
   ("C-SPC" . ace-jump-word-mode)
   ;; Needed for terminal.
   ("C-@" . ace-jump-word-mode))
  :config
  (progn
    (hook-fn 'ace-jump-mode-end-hook
      (ignore-errors
        (exit-recursive-edit)))

    ;; Use ESC to quit ace-jump.
    (--each '(ace-jump-line-mode ace-jump-word-mode ace-jump-char-mode)
      (hook-fn it (local-set-key (kbd "ESC") 'keyboard-quit)))))

(use-package hideshow
  :diminish hs-minor-mode
  :commands hs-minor-mode
  :defer    t)

(use-package abbrev
  :defer t
  :config
  (setq abbrev-file-name (concat cb:tmp-dir "abbrev_defs")))

(provide 'cb-productivity)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-productivity.el ends here
