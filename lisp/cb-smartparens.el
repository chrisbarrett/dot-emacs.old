;;; cb-smartparens.el --- Configuration for Smartparens

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130708.0124

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

;; Configuration for Smartparens

;;; Code:

(require 'use-package)
(require 'dash)
(autoload 'org-metadown "org")
(autoload 'org-metaup "org")

(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-mode +1)
    (show-smartparens-global-mode +1)

    ;; Still use Paredit wrap commands.
    (define-key sp-keymap (kbd "M-{") 'paredit-wrap-curly)
    (define-key sp-keymap (kbd "M-[") 'paredit-wrap-square)
    (define-key sp-keymap (kbd "M-(") 'paredit-wrap-round)

    ;; DEL will delete unbalanced parens.
    (define-key sp-keymap (kbd "DEL")
      (command (sp-backward-delete-char (or _arg 1))))

    ;; C-k kills blank lines or balanced sexps.
    (define-key sp-keymap (kbd "C-k")
      (command (if (emr-blank-line?)
                   (kill-whole-line)
                 (sp-kill-sexp))))

    (define-key sp-keymap (kbd "C-<backspace>") 'sp-backward-up-sexp)

    (defun sp-insert-or-up (delim &optional arg)
      "Insert a delimiter DELIM if inside a string, else move up."
      (interactive "sDelimiter:\nP")
      (if (or (emr-looking-at-string?)
              (emr-looking-at-comment?))
          (insert delim)
        (sp-up-sexp arg)))

    ;; Close paren keys move up sexp.
    (setq sp-navigate-close-if-unbalanced t)
    (--each '(")" "]" "}")
      (global-set-key (kbd it) (command (sp-insert-or-up it _arg))))

    ;; Do splices with meta up/down, except in Org mode.
    (define-key sp-keymap (kbd "M-<up>")
      (command (if (derived-mode-p 'org-mode)
                   (org-metaup)
                 (sp-splice-sexp-killing-backward 1))))
    (define-key sp-keymap (kbd "M-<down>")
      (command (if (derived-mode-p 'org-mode)
                   (org-metadown)
                 (sp-splice-sexp-killing-forward))))))

(provide 'cb-smartparens)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-smartparens.el ends here
