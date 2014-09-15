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
(require 'config-modegroups)

(cb:install-package 'evil t)
(cb:install-package 'evil-numbers t)
(cb:install-package 'evil-surround t)
(evil-mode +1)
(global-evil-surround-mode +1)

;;; Autoloads

(autoload 'ispell-add-to-dict "cb-spelling")
(autoload 'ispell-add-per-file-word-list "ispell")

;;; Variables

(custom-set-variables
 '(evil-want-visual-char-semi-exclusive t)
 '(evil-default-cursor t)
 '(evil-shift-width 2)
 '(evil-symbol-word-search 'symbol))

;;; Set initial states

(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'message-mode 'insert)
(evil-set-initial-state 'magit-commit-mode 'normal)
(evil-set-initial-state 'git-rebase-mode 'insert)
(evil-set-initial-state 'eshell-mode 'insert)
(evil-set-initial-state 'haskell-error-mode 'emacs)
(evil-set-initial-state 'doc-view-mode 'normal)
(evil-set-initial-state 'idris-info-mode 'emacs)
(evil-set-initial-state 'vkill-mode 'emacs)

;;; HJKL bindings

(evil-add-hjkl-bindings tar-mode-map)
(evil-add-hjkl-bindings occur-mode-map)
(evil-add-hjkl-bindings archive-mode-map)
(evil-add-hjkl-bindings package-menu-mode-map)

;;; Configure evil-numbers

(define-key evil-normal-state-map (kbd "C-A")   'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-A") 'evil-numbers/dec-at-pt)

;;; Configure general key bindings

(defmacro evil-global-set-keys (state &rest defs)
  "Variadic version of `evil-global-set-key'.
Creates STATE bindings for DEFS.
DEFS are comprised of alternating string-symbol pairs."
  (declare (indent 1))
  `(after 'evil
     ,@(-map (lambda+ ((key fn))
               `(evil-global-set-key ,state (kbd ,key) ,fn))
             (-partition-all 2 defs))))

(defun evil-undefine ()
  "Call the underlying command as if evil mode were not active."
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(define-keys evil-normal-state-map
  "TAB" 'indent-according-to-mode
  "<backtab>" 'outdent
  "SPC" 'evil-toggle-fold
  "K"   'cbevil:get-documentation
  "u"   'undo-tree-undo
  "C-R" 'undo-tree-redo
  "C-S-P" 'evil-paste-pop-next)

(define-key evil-insert-state-map (kbd "C-z") 'evil-undefine)
(define-key evil-emacs-state-map  (kbd "M-z") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-z") 'evil-undefine)

;;; Spelling commands

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

;;; Documentation search

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

;;; Make window management work for all modes

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

;;; Undo-tree

(after 'undo-tree
  ;; Ensure undo-tree commands are remapped. The referenced keymap in
  ;; evil-integration is incorrect.
  (define-keys undo-tree-visualizer-mode-map
    [remap evil-backward-char] 'undo-tree-visualize-switch-branch-left
    [remap evil-forward-char]  'undo-tree-visualize-switch-branch-right
    [remap evil-next-line]     'undo-tree-visualize-redo
    [remap evil-previous-line] 'undo-tree-visualize-undo))

(evil-global-set-key 'insert (kbd "S-TAB") 'tab-to-tab-stop)

;;; Configure evil-surround.

(setq-default evil-surround-pairs-alist
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

;;; Elisp

;; Make M-. work in normal state.
(evil-define-key 'normal emacs-lisp-mode-map
  (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

(hook-fn 'cb:elisp-modes-hook
  (make-local-variable 'evil-surround-pairs-alist)
  (push '(?\` . ("`" . "'")) evil-surround-pairs-alist))

(defun cbevil:get-elisp-doc ()
  (let ((sym (symbol-at-point)))
    (cond
     ((symbol-function sym)
      (describe-function sym))
     ((and (boundp sym) (not (facep sym)))
      (describe-variable sym))
     ((facep sym)
      (describe-face sym))
     (t
      (user-error "No documentation available")))))

(define-evil-doc-handler cb:elisp-modes (cbevil:get-elisp-doc))

;;; Ruby

(define-evil-doc-handler cb:ruby-modes (call-interactively 'robe-doc))

;;; Clojure

(define-evil-doc-handler cb:clojure-modes (call-interactively 'cider-doc))

;;; Python

(define-evil-doc-handler 'cb:python-modes (call-interactively 'rope-show-doc))

(evil-define-key 'normal python-mode-map (kbd "M-.") 'rope-goto-definition)

;;; Scheme

(define-evil-doc-handler cb:scheme-modes
  (call-interactively 'geiser-doc-symbol-at-point))

;;; Org

(evil-declare-key 'normal org-mode-map
  "gk"  'outline-up-heading
  "gj"  'outline-next-visible-heading
  "H"   'org-beginning-of-line ; smarter behaviour on headlines etc.
  "L"   'org-end-of-line ; smarter behaviour on headlines etc.
  "t"   'org-todo ; mark a TODO item as DONE
  ",a"  'org-attach
  ",c"  'cb-org:copy-subtree-to
  ",d"  'org-cut-special
  ",e"  'org-export-dispatch
  ",n"  'outline-next-visible-heading
  ",p"  'outline-previous-visible-heading
  ",r"  'org-refile
  ",t"  'org-set-tags-command
  ",u"  'outline-up-heading
  ",y"  'org-paste-special
  "$"   'org-end-of-line ; smarter behaviour on headlines etc.
  "^"   'org-beginning-of-line ; ditto
  "-"   'org-ctrl-c-minus ; change bullet style
  "<"   'org-metaleft ; out-dent
  ">"   'org-metaright ; indent
  )

(hook-fn 'org-agenda-mode-hook
  (smartparens-mode -1))

(add-hook 'org-capture-mode-hook 'cb:maybe-evil-insert-state)

(hook-fn 'org-mode-hook
  (when (equal (buffer-name) "*Org Note*")
    (cb:maybe-evil-insert-state)))

(after 'org-agenda
  (bind-keys
    :map org-agenda-mode-map
    "C" 'org-agenda-capture
    "g" 'org-agenda-goto-date
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    "L" 'org-agenda-log-mode
    "l" 'evil-forward-char
    "h" 'evil-backward-char
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up))

(defadvice org-insert-heading (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-insert-heading-respect-content (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-insert-todo-heading (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-insert-todo-heading-respect-content (after insert-state activate)
  (when (called-interactively-p nil)
    (cb:maybe-evil-insert-state)))

(defadvice org-toggle-heading (after goto-line-end activate)
  "Prevent point from being moved to the line beginning."
  (when (s-matches? (rx bol (+ "*") (* space) eol) (current-line))
    (goto-char (line-end-position))))

(defun cborg-evil-fold ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-global-cycle 1)
    (recenter)))

(defun cborg-evil-reveal ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-global-cycle 16)
    (recenter)))

(evil-define-key 'normal org-mode-map
  (kbd "<return>") 'org-return
  (kbd "M-P") 'outline-previous-visible-heading
  (kbd "M-N") 'outline-next-visible-heading
  (kbd "SPC") 'org-cycle
  (kbd "z m") 'cborg-evil-fold
  (kbd "z r") 'cborg-evil-reveal)

(defadvice org-return (around newlines-only-in-insert-state activate)
  "Only insert newlines if we're in insert state."
  (noflet ((newline (&rest args)
                    (when (and (fboundp 'evil-insert-state-p)
                               (evil-insert-state-p))
                      (funcall this-fn args))))
    ad-do-it))

;;; Ledger

(evil-define-key 'normal ledger-report-mode-map
  "e" 'ledger-report-edit
  "q" 'cb:go-away-buffer
  "r" 'ledger-report-redo
  "R" 'ledger-report-reverse-lines
  "s" 'ledger-report-save)

;;; Haskell

(evil-define-key 'normal haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

(defun cb-hs:join-line ()
  (interactive)
  (forward-line 1)
  (goto-char (line-beginning-position))
  (call-interactively 'shm/delete-indentation))

(evil-define-key 'normal shm-map "J" 'cb-hs:join-line)

;;; Spray

(evil-set-initial-state 'spray-mode 'emacs)

(defun cbevil:set-spray-mode-state ()
  "Set correct evil state for spray mode."
  (cond ((true? spray-mode)
         (evil-mode -1)
         )
        (t
         (evil-mode +1)
         (evil-normal-state))))

(add-hook 'spray-mode-hook 'cbevil:set-spray-mode-state)

;;; Docview

(evil-define-key 'normal doc-view-mode-map
  "j" 'doc-view-scroll-up-or-next-page
  "k" 'doc-view-scroll-down-or-previous-page
  "-" 'doc-view-shrink
  "+" 'doc-view-enlarge)

;;; TeX

(evil-define-key 'normal TeX-mode-map
  (kbd "z m") 'TeX-fold-buffer
  (kbd "z r") 'TeX-fold-clearout-buffer
  (kbd "SPC") 'TeX-fold-dwim)

;;; Magit

(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)

(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)

(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

(evil-define-key 'normal magit-commit-mode-map
  "q" 'magit-mode-quit-window)

(evil-global-set-keys 'normal
  "g P" 'magit-key-mode-popup-pushing
  "g c" 'magit-key-mode-popup-committing
  "g l" 'magit-log
  "g r" 'magit-reflog
  "g D" 'magit-diff-working-tree
  "g B" 'magit-blame-mode
  "g b" (command
         (with-window-restore
           (magit-branch-manager)
           (buffer-local-set-key (kbd "q") (command (restore))))))

(after 'magit
  (define-keys magit-diff-mode-map
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up
    "j"   'evil-next-line
    "k"   'evil-previous-line
    "/"   'evil-search-forward))

(evil-global-set-keys 'normal
  "g n" (lambda (arg)
          (interactive "p")
          (git-gutter+-refresh)
          (git-gutter+-next-hunk arg))
  "g p" (lambda (arg)
          (interactive "p")
          (git-gutter+-refresh)
          (git-gutter+-previous-hunk arg))
  "g h" 'git-gutter+-popup-hunk
  "g x" 'git-gutter+-revert-hunk
  "g s" 'git-gutter+-stage-hunks
  "g a" 'cb-git:add)

;;; Rust

(evil-define-key 'normal rust-mode-map
  (kbd "M-.") 'racer-find-definition)

;;; Ediff

(add-hook 'ediff-startup-hook 'turn-off-evil-mode)

(evil-define-key 'normal diff-mode-map (kbd "q") 'cb-diff:close)

;;; Smartparens

(evil-global-set-key 'normal "(" 'sp-backward-up-sexp)
(evil-global-set-key 'normal ")" 'sp-forward-sexp)

(evil-define-state paren "Paren editing state."
  :tag " <P> "
  :message "-- PAREN --"
  :suppress-keymap t
  :cursor 'hollow)

(hook-fn 'evil-paren-state-entry-hook
  (when (equal last-command 'evil-end-of-line)
    (forward-char)))

;; Configure entry and exit from paren state.
(evil-global-set-key 'normal (kbd ",") 'evil-paren-state)
(define-key evil-paren-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-paren-state-map (kbd "C-g") 'evil-normal-state)
;; Define paren state keys.
(evil-global-set-keys 'paren
  "A" 'sp-add-to-previous-sexp
  "a" 'sp-add-to-next-sexp
  "B" 'sp-backward-barf-sexp
  "b" 'sp-forward-barf-sexp
  "M" 'sp-backward-slurp-sexp
  "m" 'sp-forward-slurp-sexp
  "c" 'sp-convolute-sexp
  "D" 'sp-backward-kill-sexp
  "d" 'sp-kill-sexp
  "e" 'sp-emit-sexp
  "G" 'sp-end-of-sexp
  "g" 'sp-beginning-of-sexp
  "j" 'sp-join-sexp
  "K" 'sp-splice-sexp-killing-backward
  "k" 'sp-splice-sexp-killing-forward
  "n" 'sp-next-sexp
  "p" 'sp-previous-sexp
  "r" 'sp-raise-sexp
  "s" 'sp-splice-sexp-killing-around
  "t" 'sp-transpose-sexp
  "U" 'sp-backward-unwrap-sexp
  "u" 'sp-unwrap-sexp
  "w" 'sp-rewrap-sexp
  "x" 'sp-split-sexp
  "Y" 'sp-backward-copy-sexp
  "y" 'sp-copy-sexp
  "," 'sp-previous-sexp
  "." 'sp-next-sexp
  "<" 'sp-backward-down-sexp
  ">" 'sp-down-sexp)

;;; Helm

(bind-key "C-SPC" 'helm-mini)
(bind-key "C-x C-b" 'helm-buffers-list)

(evil-global-set-key 'normal (kbd "C-e") 'helm-etags-select)
(evil-global-set-key 'normal (kbd "C-t") 'helm-imenu)

;;; Yasnippet

(defadvice yas-prev-field (after insert-state activate)
  (cb:maybe-evil-insert-state))

(defadvice yas-prev-field (after insert-state activate)
  (cb:maybe-evil-insert-state))

(add-hook 'yas-before-expand-snippet-hook 'cb:maybe-evil-insert-state)

;;; BBDB

(evil-define-key 'normal bbdb-mode-map
  "j" 'bbdb-next-record
  "k" 'bbdb-prev-record
  "l" 'bbdb-next-field
  "h" 'bbdb-prev-field)

;;; Dired

(evil-define-key 'normal dired-mode-map (kbd "SPC") 'dired-hide-subdir
  (kbd "TAB") 'dired-hide-subdir
  [backtab] 'dired-hide-all
  [backspace] 'dired-kill-subdir)

(bind-key* "M-d" 'dired-jump)
(bind-key* "M-D" 'dired-jump-other-window)

;;; Discover

(evil-set-initial-state 'makey-key-mode 'emacs)

;;; Info

(evil-define-key 'motion Info-mode-map
  (kbd "l") 'Info-history-back
  (kbd "r") 'Info-history-forward)

;;; Circe

(evil-set-initial-state 'circe-server-mode 'insert)
(evil-set-initial-state 'circe-channel-mode 'insert)
(evil-set-initial-state 'circe-chat-mode 'insert)

;;; Markdown

(evil-define-key 'normal markdown-mode-map
  (kbd "M-P") 'outline-previous-visible-heading
  (kbd "M-N") 'outline-next-visible-heading)

;;; Scala

(evil-define-key 'normal scala-mode-map "J" 'cbscala:join-line)

;;; Scheme

(evil-define-key 'normal geiser-mode-map
  (kbd "M-.") 'geiser-edit-symbol-at-point)

(evil-set-initial-state 'geiser-repl-mode 'insert)

;;; w3m
(after 'w3m
  (bind-keys
    :map w3m-mode-map
    "z t" 'evil-scroll-line-to-top
    "z b" 'evil-scroll-line-to-bottom
    "z z" 'evil-scroll-line-to-center
    "C-f" 'evil-scroll-page-down
    "C-b" 'evil-scroll-page-up
    "w"   'evil-forward-word-begin
    "b"   'evil-backward-word-begin
    "y"   'evil-yank
    "p"   'evil-paste-after
    "/"   'evil-search-forward
    "?"   'evil-search-backward
    "n"   'evil-search-next
    "N"   'evil-search-previous))

;;; Misc

(evil-define-key 'normal Man-mode-map (kbd "q") 'Man-kill)

(provide 'config-evil)

;;; config-evil.el ends here
