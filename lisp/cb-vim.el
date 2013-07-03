;;; cb-vim.el --- Configuration for Evil and Vim

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0009

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

;; Configuration for Evil and Vim

;;; Code:

(require 'use-package)

(autoload 'woman-file-name-all-completions "woman")
(autoload 'Man-getpage-in-background "man")

(defun get-manpage (candidate)
  "Show the manpage for CANDIDATE."
  (let ((wfiles (mapcar 'car (woman-file-name-all-completions candidate))))
    (condition-case _
        (if (> (length wfiles) 1)
            (woman-find-file
             (helm-comp-read
              "ManFile: " wfiles :must-match t))
          (woman candidate))
      ;; If woman is unable to format correctly
      ;; use man instead.
      (error
       (kill-buffer)
       (Man-getpage-in-background candidate)))))

(defun get-elisp-doc (sym)
  "Find the appropriate documentation for SYM."
  (cond
   ((symbol-function sym) (describe-function sym))
   ((facep sym)           (describe-face sym))
   ((boundp sym)          (describe-variable sym))
   (t                     (user-error "No documentation available"))))

(defun* get-documentation (&optional (candidate (thing-at-point 'symbol)))
  "Get documentation for CANDIDATE."
  (interactive)
  ;;
  (cl-flet ((derived? (list-symbol)
                      (when (boundp list-symbol)
                        (apply 'derived-mode-p (eval list-symbol)))))
    (condition-case _
        (cond
         ((derived? 'cb:elisp-modes)
          (get-elisp-doc (intern candidate)))
         ((derived? 'cb:ruby-modes)
          (call-interactively 'robe-doc))
         ((derived? 'cb:python-modes)
          (call-interactively 'jedi-show-doc))
         (t
          (get-manpage candidate)))
      (error
       (user-error "No documentation available")))))

(use-package evil
  :ensure   t
  :commands evil-mode
  :init
  (add-hook 'after-init-hook 'evil-mode)
  :config
  (progn

    (defun cb:append-buffer ()
      "Enter insertion mode at the end of the current buffer."
      (interactive)
      (goto-char (point-max))
      (when (fboundp 'evil-append-line)
        (evil-append-line 1)))

    (defun evil-undefine ()
      (interactive)
      (let (evil-mode-map-alist)
        (call-interactively (key-binding (this-command-keys)))))

    (define-key evil-normal-state-map (kbd "M-z") 'evil-emacs-state)
    (define-key evil-emacs-state-map  (kbd "M-z") 'evil-normal-state)
    (define-key evil-normal-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
    (define-key evil-insert-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-visual-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-normal-state-map (kbd "K")   'get-documentation)

    (after "man"
      (evil-declare-key 'normal Man-mode-map (kbd "q") 'Man-kill))

    (after "org"
      (evil-define-key 'normal org-mode-map (kbd "SPC")
        'org-cycle)
      (evil-define-key 'normal org-mode-map (kbd "z m")
        (command (org-global-cycle 1)))
      (evil-define-key 'normal org-mode-map (kbd "z r")
        (command (org-global-cycle 0))))

    (after "tar-mode"
      (evil-add-hjkl-bindings tar-mode-map))

    (after "arc-mode"
      (evil-add-hjkl-bindings archive-mode-map))

    (after "undo-tree"
      ;; Ensure undo-tree commands are remapped. The referenced keymap in
      ;; evil-integration is incorrect.
      (define-key undo-tree-visualizer-mode-map [remap evil-backward-char]
        'undo-tree-visualize-switch-branch-left)
      (define-key undo-tree-visualizer-mode-map [remap evil-forward-char]
        'undo-tree-visualize-switch-branch-right)
      (define-key undo-tree-visualizer-mode-map [remap evil-next-line]
        'undo-tree-visualize-redo)
      (define-key undo-tree-visualizer-mode-map [remap evil-previous-line]
        'undo-tree-visualize-undo))

    (evil-global-set-key 'insert (kbd "S-TAB") 'tab-to-tab-stop)

    ;; Use ESC as quit command in most situations.
    (--each '(evil-normal-state-map
              evil-visual-state-map
              minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
      (define-key (eval it) [escape] 'keyboard-quit))

    (setq evil-want-visual-char-semi-exclusive t
          evil-toggle-key (kbd "M-z")
          evil-default-cursor t)
    (setq-default evil-shift-width 2)

    (evil-mode +1)))

(use-package surround
  :ensure t
  :defer  t
  :idle   (require 'surround)
  :init   (after 'evil (require 'surround))
  :config
  (progn
    (global-surround-mode +1)
    (hook-fn 'prog-mode-hook
      (push '(?\( . ("(" . ")")) surround-pairs-alist)
      (push '(?\[ . ("[" . "]")) surround-pairs-alist)
      (push '(?<  . ("<" . ">")) surround-pairs-alist))
    (hook-fn 'cb:lisp-modes-hook
      (push '(?\{ . ("{" . "}")) surround-pairs-alist)
      (push '(?\` . ("`" . "'")) surround-pairs-alist))))

(use-package evil-numbers
  :ensure t
  :commands
  (evil-numbers/dec-at-pt
   evil-numbers/inc-at-pt)
  :init
  (after 'evil
    (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
    (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)))

(provide 'cb-vim)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-vim.el ends here
