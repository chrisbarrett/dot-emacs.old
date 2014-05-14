;;; config-evil.el --- Configuration for evil

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

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

;; Configuration for evil

;;; Code:

(require 'utils-common)

(cb:install-package 'evil t)

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
(setq-default evil-symbol-word-search 'symbol)

(defmacro evil-global-set-keys (state &rest defs)
  "Variadic version of `evil-global-set-key'
Creates STATE bindings for DEFS. DEFS are comprised of alternating string-symbol pairs."
  (declare (indent 1))
  `(after 'evil
     ,@(-map (lambda+ ((key fn))
               `(evil-global-set-key ,state (kbd ,key) ,fn))
             (-partition-all 2 defs))))

(evil-mode +1)

(defun evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(define-keys evil-normal-state-map
  "TAB" 'indent-according-to-mode
  "<backtab>" 'outdent
  "M-z" 'evil-emacs-state
  "C-z" 'evil-undefine
  "SPC" 'evil-toggle-fold
  "K"   'cbevil:get-documentation
  "u"   'undo-tree-undo
  "C-R" 'undo-tree-redo)

(define-key evil-insert-state-map (kbd "C-z") 'evil-undefine)
(define-key evil-emacs-state-map  (kbd "M-z") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-z") 'evil-undefine)

(autoload 'ispell-add-to-dict "cb-spelling")
(autoload 'ispell-add-per-file-word-list "ispell")

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

(define-keys evil-normal-state-map
  "[s"  'evil-previous-spelling-error
  "]s"  'evil-next-spelling-error
  "z g" 'evil-mark-word-as-good
  "z G" 'evil-mark-word-as-locally-good
  "z =" 'evil-correct-word
  "z u" 'flyspell-auto-correct-word)

(autoload 'Man-getpage-in-background "man")
(autoload 'woman-file-name-all-completions "woman")

(defvar evil-find-doc-hook nil
  "Hook run when finding documentation for the symbol at point.
Each handler should take the search string as an argument.")

(defmacro define-evil-doc-handler (modes &rest body)
  "Register a doc lookup function for MODES.

- MODES is a quoted symbol or list of symbols representing the
  modes in which this handler will be used.

- BODY are the forms to execute to show documentation."
  (let* ((modes (-listify (eval modes)))
         (fname (intern (format "cbevil:doc-search-%s" (car modes)))))
    (cl-assert modes nil "Must provide a mode or list of modes")
    (cl-assert (-all? 'symbolp modes))
    `(progn

       (defun ,fname ()
         ,(concat "Documentation search function for the following modes:"
                  "\n\n  - "
                  (s-join "\n\n  - " (-map 'symbol-name modes)))

         (when (apply 'derived-mode-p ',modes)
           ,@body
           major-mode))

       (add-hook 'evil-find-doc-hook ',fname))))

(defun get-manpage (candidate)
  "Show the manpage for CANDIDATE."
  (let ((wfiles (mapcar 'car (woman-file-name-all-completions candidate))))
    (condition-case _
        (if (> (length wfiles) 1)
            (woman-find-file
             (helm-comp-read
              "ManFile: " wfiles :must-match t))
          (woman candidate))
      ;; If woman is unable to format correctly use man instead.
      (error
       (kill-buffer)
       (Man-getpage-in-background candidate)))
    t))

(defun cbevil:get-documentation ()
  "Get documentation for string CANDIDATE.
Runs each handler added to `evil-find-doc-hook' until one of them returns non-nil."
  (interactive)
  (condition-case-unless-debug _
      (or (run-hook-with-args-until-success 'evil-find-doc-hook)
          (get-manpage (thing-at-point 'symbol)))
    (error
     (user-error "No documentation available"))))

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

(evil-add-hjkl-bindings tar-mode-map)
(evil-add-hjkl-bindings occur-mode-map)
(evil-add-hjkl-bindings archive-mode-map)
(evil-add-hjkl-bindings package-menu-mode-map)

(after 'man
  (evil-define-key 'normal Man-mode-map (kbd "q") 'Man-kill))

(add-hook 'message-mode-hook 'cb:maybe-evil-append-line)

(after 'undo-tree
  ;; Ensure undo-tree commands are remapped. The referenced keymap in
  ;; evil-integration is incorrect.
  (define-keys undo-tree-visualizer-mode-map
    [remap evil-backward-char] 'undo-tree-visualize-switch-branch-left
    [remap evil-forward-char]  'undo-tree-visualize-switch-branch-right
    [remap evil-next-line]     'undo-tree-visualize-redo
    [remap evil-previous-line] 'undo-tree-visualize-undo))

(evil-global-set-key 'insert (kbd "S-TAB") 'tab-to-tab-stop)

(cb:install-package 'surround t)

(setq-default surround-pairs-alist
              '((?\( . ("(" . ")"))
                (?\[ . ("[" . "]"))
                (?\{ . ("{" . "}"))

                (?\) . ("(" . ")"))
                (?\] . ("[" . "]"))
                (?\} . ("{" . "}"))

                (?# . ("#{" . "}"))
                (?b . ("(" . ")"))
                (?B . ("{" . "}"))
                (?> . ("<" . ">"))
                (?t . surround-read-tag)
                (?< . surround-read-tag)
                (?f . surround-function)))

(hook-fn 'cb:elisp-modes-hook
  (make-local-variable 'surround-pairs-alist)
  (push '(?\` . ("`" . "'")) surround-pairs-alist))

(global-surround-mode +1)

(cb:install-package 'evil-numbers t)

(define-keys evil-normal-state-map
  "C--" 'evil-numbers/dec-at-pt
  "C-+" 'evil-numbers/inc-at-pt)

;(cb:install-package 'evil-exchange t)
;(evil-exchange-install)

(provide 'config-evil)

;;; config-evil.el ends here
