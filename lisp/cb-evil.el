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
(eval-when-compile
  (require 'flyspell)
  (require 'evil nil t))

(autoload 'Man-getpage-in-background "man")
(autoload 'ispell-add-per-file-word-list "ispell")
(autoload 'woman-file-name-all-completions "woman")
(autoload 'ispell-add-to-dict "cb-spelling")

(defvar evil-find-doc-hook nil
  "Hook run when finding documentation for the symbol at point.
Each handler should take the search string as an argument.")

;;; Spelling commands.

(defun evil-mark-word-as-good (word)
  "Add WORD at point to the Ispell dictionary."
  (interactive (list (thing-at-point 'word)))
  (ispell-add-to-dict word)
  (message "%s added to dictionary" (s-upcase word)))

(defun evil-correct-word (arg)
  "Corect the word at point with Ispell.
With a number ARG, select the nth replacement."
  (interactive "*P")
  (if (numberp arg)
      (dotimes (_ (1+ arg))
        (flyspell-auto-correct-word))
    (ispell-word)))

(defun evil-mark-word-as-locally-good (word)
  "Add WORD at point to the list of locally-defined words."
  (interactive (list (thing-at-point 'word)))
  (when word
    (ispell-add-per-file-word-list word)
    (message "%s added to local word list" (s-upcase word))))

(defun cbevil:error-backward-search-start-pos (pos)
  "Wrap the search to the end of the buffer if there are no
errors before POS."
  (if (and (eq (current-buffer) flyspell-old-buffer-error)
           (eq pos flyspell-old-pos-error))
      (cond
       ((= flyspell-old-pos-error (point-min))
        (message "Restarting from end of buffer")
        (point-max))
       (t
        (save-excursion
          (forward-word -1)
          (point))))
    (point)))

(defun cbevil:prev-spelling-error-pos ()
  (let ((pos (cbevil:error-backward-search-start-pos (point))))
    (while (and (> pos (point-min))
                (-none? 'flyspell-overlay-p (overlays-at pos)))
      (cl-decf pos))
    pos))

(defun evil-previous-spelling-error ()
  "Go to the previous flyspell error."
  (interactive)
  (let ((pos (cbevil:prev-spelling-error-pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (when (= pos (point-min))
      (message "No more spelling errors"))))

(defun cbevil:error-forward-search-start-pos (pos)
  "Wrap the search to the beginning of the buffer if there are no
errors forward of POS."
  (if (and (eq (current-buffer) flyspell-old-buffer-error)
           (eq pos flyspell-old-pos-error))
      (cond
       ((= flyspell-old-pos-error (point-max))
        (message "Restarting from beginning of buffer")
        (point-min))
       (t
        (save-excursion
          (forward-word 1)
          (point))))
    (point)))

(defun cbevil:next-spelling-error-pos ()
  (let ((pos (cbevil:error-forward-search-start-pos (point))))
    (while (and (< pos (point-max))
                (-none? 'flyspell-overlay-p (overlays-at pos)))
      (cl-incf pos))
    pos))

(defun evil-next-spelling-error ()
  "Go to the next flyspell error."
  (interactive)
  (let ((pos (cbevil:next-spelling-error-pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (when (= pos (point-max))
      (message "No more spelling errors"))))

;;; Mode-appropriate documentation search with K.

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
       (Man-getpage-in-background candidate)))
    t))

(cl-defun cbevil:get-documentation (&optional (candidate (thing-at-point 'symbol)))
  "Get documentation for string CANDIDATE.
Runs each handler added to `evil-find-doc-hook' until one of them returns non-nil."
  (interactive)
  (condition-case-unless-debug _
      (or (run-hook-with-args-until-success 'evil-find-doc-hook candidate)
          (get-manpage candidate))
    (error
     (user-error "No documentation available"))))

(use-package evil
  :ensure   t
  :if (true? cb:use-vim-keybindings?)
  :config
  (progn

    (defmacro evil-global-set-keys (state &rest defs)
      "Variadic version of `evil-global-set-key'
Creates STATE bindings for DEFS. DEFS are comprised of alternating string-symbol pairs."
      (declare (indent 1))
      `(after 'evil
         ,@(--map `(evil-global-set-key ,state (kbd ,(car it)) ,(cadr it))
                  (-partition-all 2 defs))))

    (defun evil-undefine ()
      (interactive)
      (let (evil-mode-map-alist)
        (call-interactively (key-binding (this-command-keys)))))

    (define-keys evil-normal-state-map
      "TAB" 'indent-according-to-mode
      "M-z" 'evil-emacs-state
      "C-z" 'evil-undefine
      "SPC" 'evil-toggle-fold
      "K"   'cbevil:get-documentation
      "u"   'undo-tree-undo
      "C-R" 'undo-tree-redo
      "[s"  'evil-previous-spelling-error
      "]s"  'evil-next-spelling-error
      "z g" 'evil-mark-word-as-good
      "z G" 'evil-mark-word-as-locally-good
      "z =" 'evil-correct-word
      "z SPC" 'flyspell-auto-correct-word)

    (define-key evil-insert-state-map (kbd "C-z") 'evil-undefine)
    (define-key evil-emacs-state-map  (kbd "M-z") 'evil-normal-state)
    (define-key evil-visual-state-map (kbd "C-z") 'evil-undefine)

;;; Emacs-state window management

    ;; Make window-management the same as evil in emacs state.

    (define-prefix-command 'cb:evil-window-emu)
    (global-set-key (kbd "C-w") 'cb:evil-window-emu)
    (bind-keys
      :overriding? t
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

;;; Visual line fixes

    ;; Evil commands should respect visual-line mode and operate on the visible
    ;; line endings rather than logical lines.

    (cl-loop
     for (key cmd) in
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
      (list
       (save-excursion
         (beginning-of-visual-line)
         (point))

       (save-excursion
         ;; Move to the next line. If on the last line, take end of line instead.
         (if (equal (line-number-at-pos) (buffer-length-lines))
             (goto-char (point-max))
           (beginning-of-visual-line (1+ (or count 1))))

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
  :if (true? cb:use-vim-keybindings?)
  :config
  (progn
    (global-surround-mode +1)
    (hook-fn 'cb:lisp-modes-hook
      (push '(?\` . ("`" . "'")) surround-pairs-alist))))

(use-package evil-numbers
  :ensure t
  :if (true? cb:use-vim-keybindings?)
  :commands
  (evil-numbers/dec-at-pt
   evil-numbers/inc-at-pt)
  :init
  (after 'evil
    (define-keys evil-normal-state-map
      "C--" 'evil-numbers/dec-at-pt
      "C-+" 'evil-numbers/inc-at-pt)))

(use-package vimrc-mode
  :ensure t
  :commands vimrc-mode
  :mode ("vimrc$" . vimrc-mode))

(provide 'cb-evil)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-evil.el ends here
