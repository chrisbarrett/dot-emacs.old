;;; cb-evil.el --- Configuration for Evil and Vim

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
(require 'cb-lib)

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
          (call-interactively 'jedi:show-doc))
         (t
          (get-manpage candidate)))
      (error
       (user-error "No documentation available")))))

(use-package evil
  :ensure   t
  :commands evil-mode
  :init
  (progn
    (add-hook 'after-init-hook 'evil-mode)

    (defmacro evil-define-keys (state keymap &rest defs)
      "Variadic version of `evil-define-key'.
Creates STATE bindings for keymap. DEFS are alternating keys and functions."
      (declare (indent 2))
      `(after 'evil
         ,@(--map `(evil-define-key ,state ,keymap (kbd ,(car it)) ,(cadr it))
                  (-partition-all 2 defs))))

    (defmacro evil-global-set-keys (state &rest defs)
      "Variadic version of `evil-global-set-key'
Creates STATE bindings for DEFS. DEFS are comprised of alternating string-symbol pairs."
      (declare (indent 1))
      `(after 'evil
         ,@(--map `(evil-global-set-key ,state (kbd ,(car it)) ,(cadr it))
                  (-partition-all 2 defs)))))
  :config
  (progn

    (defun evil-undefine ()
      (interactive)
      (let (evil-mode-map-alist)
        (call-interactively (key-binding (this-command-keys)))))

    (define-keys evil-normal-state-map
      "M-z" 'evil-emacs-state
      "C-z" 'evil-undefine
      "SPC" 'evil-toggle-fold
      "K"   'get-documentation)

    (define-key evil-insert-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-emacs-state-map  (kbd "M-z") 'evil-normal-state)
    (define-key evil-visual-state-map (kbd "C-z") 'evil-undefine)

    ;;;; Emacs-state window management

    ;; Make window-management the same as evil in emacs state.

    (define-prefix-command 'cb:evil-window-emu)
    (global-set-key (kbd "C-w") 'cb:evil-window-emu)
    (bind-keys
      :global t
      "C-w C-w" 'evil-window-prev
      "C-w C-s" 'split-window-vertically
      "C-w C-v" 'split-window-horizontally
      "C-w C-o" 'delete-other-windows
      "C-w C-c" 'delete-window
      "C-w w" 'evil-window-prev
      "C-w s" 'split-window-vertically
      "C-w v" 'split-window-horizontally
      "C-w o" 'delete-other-windows
      "C-w c" 'delete-window)

    ;;;; Visual line fixes

    ;; Evil commands should respect visual-line mode and operate on the visible
    ;; line endings rather than logical lines.

    (loop for (key cmd) in
          '(("j" evil-next-visual-line)
            ("k" evil-previous-visual-line)
            ("$" evil-end-of-visual-line)
            ("^" evil-first-non-blank-of-visual-line)
            ("0" evil-beginning-of-visual-line))
          for state in '(motion normal)
          for hook in  '(prog-mode-hook text-mode-hook)
          do (eval `(hook-fn ',hook
                      (evil-local-set-key ',state ,key ',cmd))))

    (evil-define-text-object evil-line (count &rest _)
      "Move COUNT - 1 lines down."
      (list (save-excursion
              (beginning-of-visual-line)
              (point))
            (save-excursion
              (beginning-of-visual-line (1+ (or count 1)))
              (point))))

    (defun evil-insert-line (count &optional vcount)
      "Switch to Insert state just before the first non-blank character
on the current line. The insertion will be repeated COUNT times."
      (interactive "p")
      (if evil-auto-indent
          (goto-char (max (save-excursion (back-to-indentation) (point))
                          (save-excursion (beginning-of-visual-line) (point))))
        (beginning-of-visual-line))
      (setq evil-insert-count count
            evil-insert-lines nil
            evil-insert-vcount (and vcount
                                    (> vcount 1)
                                    (list (line-number-at-pos)
                                          #'evil-first-non-blank
                                          vcount)))
      (evil-insert-state 1))

    (defun evil-append-line (count &optional vcount)
      "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times."
      (interactive "p")
      (end-of-visual-line)
      (setq evil-insert-count count
            evil-insert-lines nil
            evil-insert-vcount (and vcount
                                    (> vcount 1)
                                    (list (line-number-at-pos)
                                          #'end-of-line
                                          vcount)))
      (evil-insert-state 1))

    ;;; General config

    (evil-add-hjkl-bindings tar-mode-map)
    (evil-add-hjkl-bindings occur-mode-map)
    (evil-add-hjkl-bindings archive-mode-map)

    (after 'man
      (evil-declare-key 'normal Man-mode-map (kbd "q") 'Man-kill))

    (after 'message
      (hook-fn 'message-mode-hook (evil-append-line 1)))

    (after 'undo-tree
      ;; Ensure undo-tree commands are remapped. The referenced keymap in
      ;; evil-integration is incorrect.
      (define-keys undo-tree-visualizer-mode-map
        [remap evil-backward-char] 'undo-tree-visualize-switch-branch-left
        [remap evil-forward-char]  'undo-tree-visualize-switch-branch-right
        [remap evil-next-line]     'undo-tree-visualize-redo
        [remap evil-previous-line] 'undo-tree-visualize-undo))

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
    (setq-default surround-pairs-alist
                  (-union surround-pairs-alist
                          '((?\( . ("(" . ")"))
                            (?\[ . ("[" . "]"))
                            (?<  . ("<" . ">"))
                            (?\{ . ("{" . "}")))))

    (hook-fn 'cb:lisp-modes-hook
      (push '(?\` . ("`" . "'")) surround-pairs-alist))))

(use-package evil-numbers
  :ensure t
  :commands
  (evil-numbers/dec-at-pt
   evil-numbers/inc-at-pt)
  :init
  (after 'evil
    (define-keys evil-normal-state-map
      "C--" 'evil-numbers/dec-at-pt
      "C-=" 'evil-numbers/inc-at-pt)))

(provide 'cb-evil)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-evil.el ends here
