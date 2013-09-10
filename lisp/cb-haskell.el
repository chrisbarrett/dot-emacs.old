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
(autoload 'emr-blank-line? "emr")
(autoload 'emr-line-matches? "emr")
(autoload 'haskell-cabal-find-file "haskell-cabal")
(autoload 'smart-insert-operator "smart-operator")
(autoload 'smart-insert-operator-hook "smart-operator")

(after 'haskell-mode

;;;; Buffer switching

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

  (define-key haskell-mode-map (kbd "C-c j") 'haskell-test<->code)

;;;; Unicode display

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
                             (group (? "`") symbol-start ,pat symbol-end
                                    (? "`"))
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

(after 'flycheck
  (hook-fn 'haskell-mode-hook
    (flycheck-select-checker 'haskell-ghc)))

(after 'auto-complete
  (--map (add-to-list 'ac-modes it)
         cb:haskell-modes))

(after 'smart-operator

  (defun cb-hs:smart-pipe ()
    "Insert a pipe operator. Add padding, unless we're inside a list."
    (interactive)
    (if (s-matches? (rx "[" (* (any "|" alnum)) eol)
                    (buffer-substring (line-beginning-position) (point)))
        (insert "|")
      (smart-insert-operator "|")))

  (defun cb-hs:smart-dot ()
    "Insert a period. Add padding, unless this line is an import statement."
    (interactive)
    (if (emr-line-matches? (rx bol (or "import" "module")))
        (insert ".")
      (smart-insert-operator ".")))

  (defun cb-hs:smart-colon ()
    (interactive)
    (if (equal (char-before) ?\:)
        (atomic-change-group
          (delete-char -1)
          (just-one-space)
          (insert "::")
          (just-one-space))
      (insert ":")))

  (defun cb-hs:smart-gt ()
    "Append an arrow to the end of the line if we're in a typesig."
    (interactive)
    (let ((lin (buffer-substring (line-beginning-position) (point))))
      (if (and (s-matches? "::" lin)
               (not (s-matches? (rx (any "=" "-") (* space) eol) lin)))
          (atomic-change-group
            (end-of-line)
            (just-one-space)
            (insert "-> "))
        (smart-insert-operator ">"))))

  (hook-fn 'cb:haskell-modes-hook
    (smart-insert-operator-hook)
    (local-set-key (kbd ".") 'cb-hs:smart-dot)
    (local-set-key (kbd ">") 'cb-hs:smart-gt)
    (local-set-key (kbd ":") 'cb-hs:smart-colon)
    (local-set-key (kbd "|") 'cb-hs:smart-pipe)
    (local-set-key (kbd "$") (command (smart-insert-operator "$")))))

(after 'hideshow

  (defun cb-hs:forward-fold (&rest _)
    (haskell-ds-forward-decl)
    ;; Skip infix and import groups.
    (while (emr-line-matches? (rx bol (or "import" "infix") (+ space)))
      (haskell-ds-forward-decl))
    (unless (eobp)
      (ignore-errors (forward-line -1))
      (while (emr-line-matches? (rx bol (* space) "--"))
        (forward-line -1))))

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
      (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build))))

(use-package haskell-c
  :mode ("\\.hsc$" . haskell-c-mode))

(use-package haskell-cabal
  :mode  ("\\.cabal$" . haskell-cabal-mode))

(use-package haskell-ac
  :defer t
  :init
  (hook-fn 'cb:haskell-modes-hook
    (require 'haskell-ac)
    (after 'auto-complete
      (add-to-list 'ac-modes major-mode)
      (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
      (add-to-list 'ac-sources 'ac-source-haskell))))

(use-package haskell-edit
  :commands (haskell-find-type-signature
             haskell-reformat-type-signature))

(use-package hi2
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook 'turn-on-hi2))

(use-package haskell-doc
  :diminish haskell-doc-mode
  :commands haskell-doc-mode
  :init     (add-hook 'cb:haskell-modes-hook 'haskell-doc-mode))

(use-package haskell-decl-scan
  :commands turn-on-haskell-decl-scan
  :init     (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan))

(use-package hs-lint
  :commands hs-lint
  :init
  (hook-fn 'haskell-mode-hook
    (local-set-key (kbd "C-c l") 'hs-lint))
  :config
  (setq hs-lint-command (executable-find "hlint")))

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
