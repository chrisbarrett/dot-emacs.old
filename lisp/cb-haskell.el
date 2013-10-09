;;; cb-haskell --- Haskell editing commands and advice.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Haskell editing commands and advice.

;;; Code:

;;; Testing

(require 'use-package)
(require 's)
(require 'cb-search)
(autoload 'emr-blank-line? "emr")
(autoload 'emr-line-matches? "emr")
(autoload 'haskell-cabal-find-file "haskell-cabal")
(autoload 'smart-insert-operator "smart-operator")
(autoload 'smart-insert-operator-hook "smart-operator")

;; Add search commands for hoogle.
(cbs-define-search-method
 :name "hoogle"
 :key "h"
 :command
 (lambda (q)
   (browse-url (concat "http://www.haskell.org/hoogle/?hoogle="
                       (url-hexify-string (cbs-read "Hoogle" q)))))
 :when
 (lambda ()
   (derived-mode-p 'haskell-mode 'inf-haskell-mode)))

;; <C-c j> switches between src code and test file.
(after 'haskell-mode

  (defun cb:hs-project-root ()
    (or (ignore-errors
          (file-name-directory (haskell-cabal-find-file)))
        (projectile-project-p)))

  (defun cb:hs-srcfile->testfile (fname)
    (s-replace "/src/" "/test/"
               (replace-regexp-in-string "\\(.\\)l?hs$" "Tests." fname t nil 1)))

  (defun cb:hs-testfile->srcfile (fname)
    (s-replace "/test/" "/src/"
               (replace-regexp-in-string "\\(Tests\\).l?hs$" "" fname t nil 1)))

  (defun cb:hs-test<->code ()
    (interactive)
    (let* ((src-p (s-contains? "/src/" (buffer-file-name)))
           (file (if src-p
                     (cb:hs-srcfile->testfile (buffer-file-name))
                   (cb:hs-testfile->srcfile (buffer-file-name)))))
      (when (cb:hs-project-root)
        (cond
         ((file-exists-p file) (find-file file))
         (src-p (error "No corresponding unit test file found"))
         (t     (error "No corresponding source file found"))))))

  (define-key haskell-mode-map (kbd "C-c j") 'haskell-test<->code))

;; Use font lock to display unicode symbols in haskell buffers.
(after 'haskell-mode

  (defun cb-hs:apply-font-lock (pat rep)
    "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
    (font-lock-add-keywords
     nil `((,pat
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(string-to-char rep) 'decompose-region)
                      nil))))))

  (defun cb-hs:font-lock (patterns)
    (--each patterns
      (destructuring-bind (pat rep) it
        (cb-hs:apply-font-lock
         (rx-to-string `(and (not (any "\""))
                             (? "`")
                             (group  symbol-start ,pat symbol-end)
                             (? "`")
                             (not (any "\""))))
         rep))))

  (defun cb-hs:apply-unicode ()
    (cb-hs:apply-font-lock
     "\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->" "λ")
    (cb-hs:font-lock '(("<-"     "←")
                       ("->"     "→")
                       ("=>"     "⇒")
                       ("."      "•")
                       ("forall" "∀")
                       ("undefined" "⊥")
                       (">="     "≥")
                       ("<="     "≤")
                       ("=="     "≣")
                       ("alpha"  "ɑ")
                       ("beta"   "β")
                       ("gamma"  "ɣ")
                       ("delta"  "δ")
                       ("elem"   "∈")
                       ("notElem" "∉")
                       ("!!"     "‼")
                       ("::"     "∷"))))

  (add-hook 'cb:haskell-modes-hook 'cb-hs:apply-unicode))

;; Use ghc flymake checker because the cabal checker doesn't work for standalone files.
(after 'flycheck
  (hook-fn 'haskell-mode-hook
    (flycheck-select-checker 'haskell-ghc)))

;; Enable auto-complete in haskell buffers.
(after 'auto-complete
  (-each cb:haskell-modes (~ add-to-list 'ac-modes)))

;; Define custom smart operators for haskell.
(after 'smart-operator

  (defun cb-hs:smart-pipe ()
    "Insert a pipe operator. Add padding, unless we're inside a list."
    (interactive)
    (if (s-matches? (rx "[" (* (any "|" alnum)) eol)
                    (buffer-substring (line-beginning-position) (point)))
        (insert "|")
      (smart-insert-operator "|")))

  (defun cb-hs:looking-at-module-or-constructor? ()
    (-when-let (sym (thing-at-point 'symbol))
      (s-uppercase? (substring sym 0 1))))

  (defun cb-hs:smart-dot ()
    "Insert a period. Add padding, unless this line is an import statement."
    (interactive)
    (if (cb-hs:looking-at-module-or-constructor?)
        (insert ".")
      (smart-insert-operator ".")))

  (defun cb-hs:smart-colon ()
    "Insert either a type binding colon pair or a cons colon."
    (interactive)
    (if (s-matches? (rx bol (* space) (? ",") (* space)
                        (+ (not (any space "("))) (* space) eol)
                    (buffer-substring (line-beginning-position) (point)))
        (atomic-change-group
          (just-one-space)
          (insert "::")
          (just-one-space))
      (insert ":")))

  (defun cb-hs:insert-arrow (arrow)
    "If point is inside a tuple, insert an arrow inside.
Otherwise insert an arrow at the end of the line."
    (atomic-change-group
      (cl-destructuring-bind (&key beg end op &allow-other-keys)
          (sp-get-sexp t)
        ;; Check whether point is inside a tuple.
        (if (and (equal op "(")
                 (> (point) beg)
                 (< (point) end))
            (sp-end-of-sexp)
          (end-of-line)))
      ;; Insert arrow.
      (just-one-space)
      (insert arrow)
      (just-one-space)))

  (defun cb-hs:at-typedecl? ()
    (s-matches? "::" (buffer-substring (line-beginning-position) (point))))

  (defun cb-hs:smart-minus ()
    "Insert an arrow if we're in a typesig, otherwise perform a normal insertion."
    (interactive)
    (if (cb-hs:at-typedecl?)
        (cb-hs:insert-arrow "->")
      (smart-insert-operator "-")))

  (defun cb-hs:smart-equals ()
    "Insert an arrow if we're in a typesig, otherwise perform a normal insertion."
    (interactive)
    (if (cb-hs:at-typedecl?)
        (cb-hs:insert-arrow "=>")
      (smart-insert-operator "=")))

  (hook-fn 'cb:haskell-modes-hook
    (smart-insert-operator-hook)
    (local-set-key (kbd "-") 'cb-hs:smart-minus)
    (local-set-key (kbd "=") 'cb-hs:smart-equals)
    (local-set-key (kbd ".") 'cb-hs:smart-dot)
    (local-set-key (kbd ":") 'cb-hs:smart-colon)
    (local-set-key (kbd "|") 'cb-hs:smart-pipe)
    (local-set-key (kbd "?") (command (smart-insert-operator "?")))
    (local-set-key (kbd "$") (command (smart-insert-operator "$")))))

;; Configure Smartparens.
(after 'smartparens
  (sp-with-modes '(haskell-mode
                   inf-haskell-mode
                   haskell-cabal-mode)
    (sp-local-pair "'" "'" :actions '(:rem insert))))

;; Configure hideshow to collapse regions such as functions, imports and datatypes.
(after 'hideshow

  (defun cb-hs:next-separator-pos ()
    (save-excursion
      (when (search-forward-regexp (rx bol "---") nil t)
        (ignore-errors (forward-line -1))
        (while (and (emr-blank-line?)
                    (not (bobp)))
          (forward-line -1))
        (end-of-line)
        (point))))

  (defun cb-hs:next-decl-pos ()
    (save-excursion
      (haskell-ds-forward-decl)
      ;; Skip infix and import groups.
      (while (emr-line-matches? (rx bol (or "import" "infix") (+ space)))
        (haskell-ds-forward-decl))
      (unless (eobp)
        (ignore-errors (forward-line -1))
        (while (and (emr-line-matches? (rx bol (* space) "--" space))
                    (not (bobp)))
          (forward-line -1)))
      (point)))

  (defun cb-hs:forward-fold (&rest _)
    (let ((sep (cb-hs:next-separator-pos))
          (decl (cb-hs:next-decl-pos)))
      (goto-char (min (or sep (point-max))
                      (or decl (point-max))))))

  (add-to-list 'hs-special-modes-alist
               `(haskell-mode
                 ;; Beginning function
                 ,(rx (or
                       ;; Function
                       (group  (* nonl) (+ space) "::" (+ space ) (* nonl))
                       ;; FFI declarations.
                       (group (? "foreign") (+ space) "import")
                       ;; Groupings
                       (group (or "class" "instance" "newtype" "data")
                              (+ space) (* nonl))))
                 ;; End function
                 nil
                 ;; Comment start
                 ,(rx "{-")
                 ;; Forward-sexp function
                 cb-hs:forward-fold)))

;; `haskell-mode' provides a major-mode for editing haskell code.
(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  :config
  (progn

    (defadvice switch-to-haskell (after insert-at-end-of-line activate)
      "Enter insertion mode at the end of the line when switching to inferior haskell."
      (cb:append-buffer))

    (add-to-list 'completion-ignored-extensions ".hi")

    (hook-fn 'cb:haskell-modes-hook
      (eldoc-mode +1)
      (subword-mode +1)
      (local-set-key (kbd "C-c h") 'hoogle))

    (defun cb:switch-to-haskell ()
      "Switch to the last active Haskell buffer."
      (interactive)
      (-when-let (buf (--first-buffer (derived-mode-p 'haskell-mode)))
        (pop-to-buffer buf)))

    (hook-fn 'inferior-haskell-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-haskell))

    (hook-fn 'haskell-mode-hook
      (setq
       evil-shift-width     4
       tab-width            4
       haskell-tags-on-save t
       haskell-stylish-on-save t)
      (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build))

    (defadvice haskell-mode-after-save-handler (around ignore-warnings activate)
      "Prevent subprocess warnings from changing window state."
      (let ((inhibit-redisplay t))
        (save-window-excursion
          ad-do-it)))))

;; `haskell-c' provides a major-mode for haskell-c code.
(use-package haskell-c
  :mode ("\\.hsc$" . haskell-c-mode))

;; `haskell-cabal' provides a major-mode for cabal files.
(use-package haskell-cabal
  :mode  ("\\.cabal$" . haskell-cabal-mode))

;; `haskell-ac' provides auto-complete sources for haskell.
(use-package haskell-ac
  :defer t
  :init
  (hook-fn 'cb:haskell-modes-hook
    (require 'haskell-ac)
    (after 'auto-complete
      (add-to-list 'ac-modes major-mode)
      (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
      (add-to-list 'ac-sources 'ac-source-haskell))))

;; `haskell-edit' contains a few custom refactoring commands.
(use-package haskell-edit
  :commands (haskell-find-type-signature
             haskell-reformat-type-signature))

;; `hi2' is an improved inhentation mode for haskell.
(use-package hi2
  :ensure t
  :defer t
  :diminish hi2-mode
  :init (add-hook 'haskell-mode-hook 'turn-on-hi2))

;; `haskell-doc' provides el-doc-style minibuffer typesignatures.
(use-package haskell-doc
  :diminish haskell-doc-mode
  :commands haskell-doc-mode
  :init     (add-hook 'cb:haskell-modes-hook 'haskell-doc-mode))

;; `hs-lint' provides an emacs interface to HLint, the haskell linting tool.
(use-package hs-lint
  :commands hs-lint
  :init
  (hook-fn 'haskell-mode-hook
    (local-set-key (kbd "C-c l") 'hs-lint))
  :config
  (setq hs-lint-command (executable-find "hlint")))

;; `scion' provides IDE-style services for haskell.
(use-package scion
  :ensure t
  :commands (scion-mode)
  :init (add-hook 'cb:haskell-modes-hook 'scion-mode)
  :config
  (progn
    (define-key scion-mode-map (kbd "M-n") nil)
    (setq pp-escape-newlines t
          scion-program "~/.cabal/bin/scion-server"
          scion-completing-read-function 'ido-completing-read)))

(provide 'cb-haskell)

;;; cb-haskell.el ends here
