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

  (defun cbhs:apply-font-lock (pat rep)
    "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
    (font-lock-add-keywords
     nil `((,pat
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(string-to-char rep) 'decompose-region)
                      nil))))))

  (defun cbhs:font-lock (patterns)
    (--each patterns
      (destructuring-bind (pat rep) it
        (cbhs:apply-font-lock
         (rx-to-string `(and (not (any "\""))
                             (? "`")
                             (group  symbol-start ,pat symbol-end)
                             (? "`")
                             (not (any "\""))))
         rep))))

  (defun cbhs:apply-unicode ()
    (cbhs:apply-font-lock
     "\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->" "λ")
    (cbhs:font-lock '(("<-"     "←")
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

  (add-hook 'cb:haskell-modes-hook 'cbhs:apply-unicode))

;; Use hdevtools checker for projects and ghc checker for standalone files.
(after 'flycheck

  (defun cbhs:configure-flycheck ()
    (flycheck-select-checker
     (if (and
          (buffer-file-name)
          (locate-dominating-file
           (f-dirname (buffer-file-name))
           (C (~ -any? (~ s-matches? "\\.cabal$")) f-files)))

         'haskell-hdevtools
       'haskell-ghc)))

  (add-hook 'haskell-mode-hook 'cbhs:configure-flycheck))

;; Enable auto-complete in haskell buffers.
(after 'auto-complete
  (-each cb:haskell-modes (~ add-to-list 'ac-modes)))

;; Define custom smart operators for haskell.
(after 'smart-operator

  (defun cbhs:smart-comma ()
    (interactive)
    (cond
     ((s-matches? (rx bol (* space) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (insert ", ")
      (hi2-indent-line))
     (t
      (insert ","))))

  (defun cbhs:smart-pipe ()
    "Insert a pipe operator. Add padding, unless we're inside a list."
    (interactive)
    (if (s-matches? (rx "[" (* (any "|" alnum)) eol)
                    (buffer-substring (line-beginning-position) (point)))
        (insert "|")
      (smart-insert-operator "|")))

  (defun cbhs:looking-at-module-or-constructor? ()
    (-when-let (sym (thing-at-point 'symbol))
      (s-uppercase? (substring sym 0 1))))

  (defun cbhs:smart-dot (&optional arg)
    "Insert a period. Add padding, unless this line is an import statement.
With a prefix arg, insert a period without padding."
    (interactive "*P")
    (cond
     (arg
      (insert "."))
     ((cbhs:looking-at-module-or-constructor?)
      (insert "."))
     (t
      (smart-insert-operator "."))))

  (defun cbhs:smart-colon ()
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

  (defun cbhs:insert-arrow (arrow)
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

  (defun cbhs:at-typedecl? ()
    (s-matches? "::" (buffer-substring (line-beginning-position) (point))))

  (defun cbhs:smart-minus (&optional arg)
    "Insert an arrow if we're in a typesig, otherwise perform a normal insertion.
With a prefix arg, insert an arrow with padding at point."
    (interactive "*P")
    (cond
     (arg
      (just-one-space)
      (insert "->")
      (just-one-space))
     ((cbhs:at-typedecl?)
      (cbhs:insert-arrow "->"))
     (t
      (smart-insert-operator "-"))))

  (defun cbhs:smart-lt (&optional arg)
    "Insert a less than symbol. With a prefix arg, insert an arrow at point."
    (interactive "*P")
    (cond
     (arg
      (just-one-space)
      (insert "<-")
      (just-one-space))
     (t
      (smart-insert-operator "<"))))

  (after 'haskell-mode
    (bind-keys
      :map haskell-mode-map
      "," 'cbhs:smart-comma
      "-" 'cbhs:smart-minus
      "=" (command (smart-insert-operator "="))
      "<" 'cbhs:smart-lt
      "." 'cbhs:smart-dot
      ":" 'cbhs:smart-colon
      "|" 'cbhs:smart-pipe
      "?" (command (smart-insert-operator "?"))
      "$" (command (smart-insert-operator "$"))))

  (hook-fn 'cb:haskell-modes-hook
    (smart-insert-operator-hook)))

;; Configure Smartparens.
(after 'smartparens
  (sp-with-modes '(haskell-mode
                   inf-haskell-mode
                   haskell-cabal-mode)
    (sp-local-pair "'" "'" :actions '(:rem insert))))

;; Configure hideshow to collapse regions such as functions, imports and datatypes.
(after 'hideshow

  (defun cbhs:next-separator-pos ()
    (save-excursion
      (when (search-forward-regexp (rx bol "---") nil t)
        (ignore-errors (forward-line -1))
        (while (and (emr-blank-line?)
                    (not (bobp)))
          (forward-line -1))
        (end-of-line)
        (point))))

  (defun cbhs:next-decl-pos ()
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

  (defun cbhs:forward-fold (&rest _)
    (let ((sep (cbhs:next-separator-pos))
          (decl (cbhs:next-decl-pos)))
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
                 cbhs:forward-fold)))

;; Add haskell insertion commands to the global picker.
(after 'haskell-mode

  (defun cbhs:parse-module (str)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (search-forward-regexp (rx bol "exposed-modules: ") nil t)
        (let (start end)
          (setq start (point))
          (setq end (if (search-forward ":" nil t)
                        (progn (beginning-of-line) (point))
                      (point-max)))
          (s-split " " (buffer-substring-no-properties start end) t)))))

  (defun cbhs:haskell-modules ()
    "Get a list of all Haskell modules known to GHC."
    (->> (%-string "ghc-pkg" "dump")
      (s-split "---")
      (-mapcat 'cbhs:parse-module)
      (-map 's-trim)))

  (defun cbhs:read-import ()
    "Read a module from the user and format as an import statement."
    (interactive)
    (let ((modid
           (helm-comp-read "Import module: "
                           (sort (cbhs:haskell-modules) 'string<)
                           :volatile t
                           :must-match t))
          (qualified? (y-or-n-p "Qualified import? ")))
      (if qualified?
          (format "import qualified %s as %s" modid (read-string "As: "))
        (format "import %s" modid))))

  (defun cbhs:insert-import ()
    "Interactively insert a Haskell import statement."
    (interactive)
    (let ((import (cbhs:read-import)))
      ;; Insert the import statement.
      (emr-reporting-buffer-changes "Inserted import"
        (save-excursion
          (goto-char (point-min))

          (cond
           ;; Move directly to import statements.
           ((search-forward-regexp (rx bol "import") nil t))

           ;; Move past module declaration.
           ((search-forward "module" nil t)
            (search-forward "where")
            (forward-line)
            (beginning-of-line)
            (while (and (s-blank? (current-line))
                        (not (eobp)))
              (forward-line)))

           ;; Otherwise insert on first blank line.
           (t
            (until (or (eobp) (s-blank? (current-line)))
              (forward-line))))

          ;; Insert import statement.
          (beginning-of-line)
          (open-line 1)
          (insert import)))))

  (-each
   (let ((pred (lambda () (derived-mode-p 'haskell-mode))))
     `(("i" "Haskell Import" cbhs:insert-import ,pred)
       ("l" "Haskell Language Extension" haskell-insert-language ,pred)
       ("p" "GHC Pragma" haskell-insert-pragma ,pred)
       ("f" "GHC Flag" haskell-insert-flag ,pred)))
   (~ add-to-list 'cb:insertion-commands)))

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
