;;; config-haskell.el --- Configure Haskell

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

;; Configure Haskell

;;; Code:

(require 'utils-common)
(require 'config-flycheck)
(require 'config-smartparens)
(require 'super-smart-ops)

(cb:declare-package-installer haskell
  :match (rx "." (or "hs" "gs" "hi" "pghci" "cabal" "hsc" "hcr"))
  :packages (haskell-mode
             flycheck-haskell
             ghc
             hi2
             shm))

(add-to-list 'completion-ignored-extensions ".hi")

(custom-set-variables
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-tags-on-save t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-stylish-on-save t)
 '(haskell-program-name "ghci")
 '(haskell-process-type 'cabal-repl)
 '(haskell-interactive-prompt "\nλ> ")
 )

(hook-fn 'cb:haskell-modes-hook
  (setq-local tab-width 2)
  (setq-local evil-shift-width 2))

(hook-fn 'haskell-mode-hook
  (haskell-doc-mode +1)
  (eldoc-mode +1)
  (diminish 'haskell-doc-mode))

(defun cb-hs:file-name->module ()
  (-if-let (root (and (buffer-file-name) (projectile-project-p)))

      (->> (f-no-ext (buffer-file-name))
        (s-chop-prefix root)
        f-split
        (-drop 1)
        (--map (let ((x (substring it 0 1))
                     (xs (substring it 1)))
                 (concat (s-upcase x) xs)))
        (s-join "."))

    (s-upper-camel-case (f-no-ext (buffer-name)))))

(defun cb-hs:last-declared-type-name ()
  "Find the last type declared with `data' or `newtype'"
  (save-excursion
    (when (search-backward-regexp (rx bol (* space)
                                      (or "newtype" "data")
                                      (+ space)
                                      (group (+ word)))
                                  nil t))
    (match-string-no-properties 1)))

(defun cb-hs:last-imported-header ()
  "Find the last header imported by a foreign import decl."
  (save-excursion
    (when (search-backward-regexp (rx bol (* space)
                                      "foreign" (+ space) "import" (+ space)
                                      (* nonl)
                                      "\""
                                      (group (+ graphic)))
                                  nil t)
      (match-string-no-properties 1))))

(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(add-hook 'haskell-mode-hook 'hi2-mode)

(after 'hi2 (diminish 'hi2-mode))

(defun cb:in-cabal-project? ()
  "Find the root of the current cabal project."
  (when (and (buffer-file-name) default-directory)
    (f-traverse-upwards
     (lambda (d)
       (--any? (equal "cabal" (f-ext it)) (f-files d)))
     default-directory)))

(defun cb:ghc-init ()
  "Run `ghc-init' without setting any key bindings or running checks."
  (noflet ((define-key (&rest _))
           (ghc-check-syntax (&rest _))
           (ghc-comp-init (&rest _)
                          (when (cb:in-cabal-project?)
                            (funcall this-fn))))
    (ghc-init)))

(add-hook 'haskell-mode-hook 'cb:ghc-init)

(defun ghci ()
  "Run a standalone instance of GHCI."
  (interactive)
  ;; Start session.
  (let ((haskell-process-type 'ghci))
    (unless (haskell-session-maybe)
      (haskell-session-assign
       (noflet ((haskell-session-cabal-dir (s) default-directory))
         (haskell-session-make "ghci")))))
  ;; Switch to window.
  (haskell-interactive-bring))

(after 'haskell-mode
  (require 'ghc)
  (require 'hi2)
  (require 'shm)
  (require 'shm-case-split)
  (require 'haskell-interactive-mode)

  ;; FIX: There is an issue preventing ghc-comp from being correctly loaded.
  ;; Load this feature manually.
  (load (->> (-first (~ s-matches? "/ghc") load-path)
          f-files
          (-first (~ s-matches? "ghc-comp"))))


  (require 'ghc)

  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "M-,") 'pop-tag-mark)

  (after 'evil
    (evil-define-key 'normal haskell-mode-map
      (kbd "M-.") 'haskell-mode-tag-find
      (kbd "M-,") 'pop-tag-mark))

  (require 'w3m-haddock)
  (add-hook 'w3m-display-hook 'w3m-haddock-display)
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)

  (hook-fn 'cb:haskell-modes-hook
    (whitespace-mode -1))

  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)

  (flycheck-define-checker haskell-c-ghc
    "A Haskell C syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
    :command ("flycheck_haskell_c.sh"
              source
              ;; Include the parent directory of the current module tree, to
              ;; properly resolve local imports
              (eval (concat
                     "-i"
                     (flycheck-module-root-directory
                      (flycheck-find-in-buffer flycheck-haskell-module-re))))
              (option-flag "-no-user-package-db"
                           flycheck-ghc-no-user-package-database)
              (option-list "-package-db" flycheck-ghc-package-databases)
              (option-list "-i" flycheck-ghc-search-path s-prepend))
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ":"
              (or " " "\n    ") "Warning:" (optional "\n")
              (one-or-more " ")
              (message (one-or-more not-newline)
                       (zero-or-more "\n"
                                     (one-or-more " ")
                                     (one-or-more not-newline)))
              line-end)
     (error line-start (file-name) ":" line ":" column ":"
            (or (message (one-or-more not-newline))
                (and "\n" (one-or-more " ")
                     (message (one-or-more not-newline)
                              (zero-or-more "\n"
                                            (one-or-more " ")
                                            (one-or-more not-newline)))))
            line-end))
    :modes haskell-c-mode)

  (define-key haskell-mode-map ghc-completion-key  'ghc-complete)
  (define-key haskell-mode-map ghc-document-key    'ghc-browse-document)
  (define-key haskell-mode-map ghc-type-key        'ghc-show-type)
  (define-key haskell-mode-map ghc-info-key        'ghc-show-info)
  (define-key haskell-mode-map ghc-expand-key      'ghc-expand-th)
  (define-key haskell-mode-map (kbd "M-P") 'flymake-goto-prev-error)
  (define-key haskell-mode-map (kbd "M-N") 'flymake-goto-next-error)
  (define-key haskell-mode-map ghc-help-key        'ghc-flymake-display-errors)
  (define-key haskell-mode-map ghc-insert-key      'ghc-insert-template)
  (define-key haskell-mode-map ghc-sort-key        'ghc-sort-lines)
  (define-key haskell-mode-map ghc-check-key       'ghc-save-buffer)
  (define-key haskell-mode-map ghc-toggle-key      'ghc-flymake-toggle-command)
  (define-key haskell-mode-map ghc-module-key      'ghc-insert-module)
  (define-key haskell-mode-map ghc-hoogle-key      'haskell-hoogle)
  (define-key haskell-mode-map ghc-shallower-key   'ghc-make-indent-shallower)
  (define-key haskell-mode-map ghc-deeper-key      'ghc-make-indent-deeper)

  (after 'evil

    (hook-fn 'evil-normal-state-entry-hook
      (when (true? hi2-mode)
        (hi2-disable-show-indentations)))

    (hook-fn 'evil-insert-state-entry-hook
      (when (true? hi2-mode)
        (hi2-enable-show-indentations)))

    (hook-fn 'evil-insert-state-exit-hook
      (when (true? hi2-mode)
        (hi2-disable-show-indentations)))
    )

  (defun cb:switch-to-haskell ()
    "Switch to the last active Haskell buffer."
    (interactive)
    (-when-let (buf (--first-buffer (derived-mode-p 'haskell-mode)))
      (pop-to-buffer buf)))


  (define-key haskell-interactive-mode-map (kbd "C-c C-z") 'cb:switch-to-haskell)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)

  (defadvice haskell-mode-after-save-handler (around ignore-warnings activate)
    "Prevent subprocess warnings from changing window state."
    (let ((inhibit-redisplay t))
      (save-window-excursion
        ad-do-it)))

  (defun cb-hs:inside-parens? ()
    "Non-nil if point is inside a parenthesised expression."
    (save-excursion
      (ignore-errors
        (backward-up-list)
        (equal (char-after)
               (string-to-char "(")))))

  (defun cb-hs:in-empty-braces? ()
    "Non-nil if point is between empty square or curly braces."
    (and (s-matches? (rx (or "{" "[") (* space) eol)
                     (buffer-substring (line-beginning-position) (point)))
         (s-matches? (rx bol (* space) (or "}" "]"))
                     (buffer-substring (point) (line-end-position)))))

  (defun cb-hs:smart-pipe ()
    "Insert a pipe operator. Add padding, unless we're inside a list."
    (interactive)
    (cond
     ((s-matches? (rx "[" (* (any "|" alnum)) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (insert "|"))
     ((s-matches? (rx "--" (* space) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (just-one-space)
      (insert "|"))
     (t
      (smart-insert-op "|"))))

  (defun cb-hs:looking-at-module-or-constructor? ()
    (-when-let (sym (thing-at-point 'symbol))
      (s-uppercase? (substring sym 0 1))))

  (defun cb-hs:smart-dot ()
    "Insert a period. Add padding, unless this line is an import statement."
    (interactive)
    (cond
     ((thing-at-point-looking-at (rx digit (* space) "."))
      (save-excursion
        (search-backward ".")
        (just-one-space))
      (insert ". "))

     ((thing-at-point-looking-at (rx digit))
      (insert "."))

     ((cb-hs:looking-at-module-or-constructor?)
      (insert "."))

     ((thing-at-point-looking-at (rx (or "(" "{" "[") (* space)))
      (insert "."))

     (t
      (smart-insert-op "."))))

  (defun cb-hs:smart-hash ()
    "Insert a hash character, with special formatting behaviour for pragmas."
    (interactive "*")
    (let* ((before (buffer-substring (line-beginning-position) (point)))
           (after (buffer-substring (point) (line-end-position)))
           (in-comment? (and (s-matches? (rx "{-" (* space) eol) before)
                             (s-matches? (rx bol (* space) "-}") after))))
      (cond
       (in-comment?
        (delete-horizontal-space)
        (insert "# ")
        (save-excursion (insert " #")))
       (t
        (smart-insert-op "#")))))

  (defun cb-hs:smart-colon ()
    "Insert a colon, with context-sensitive formatting."
    (interactive)
    (cond
     ((and (cb-hs:inside-parens?)
           (s-matches? (rx ":" (* space) eol)
                       (buffer-substring (line-beginning-position) (point))))
      (save-restriction
        (narrow-to-region (line-beginning-position) (point))
        (save-excursion
          (search-backward-regexp (rx (* space) ":" (* space)))
          (delete-region (point) (point-max)))
        (insert " :: ")))

     ((cb-hs:inside-parens?)
      (insert ":"))

     (t
      (smart-insert-op ":"))))

  (defun cb-hs:space ()
    "Insert a space with context-sensitive formatting."
    (interactive)
    (cond
     ((cb-hs:in-empty-braces?)
      (just-one-space)
      (save-excursion
        (insert " ")))
     (t
      (haskell-mode-contextual-space))))

  (defun cb-hs:del ()
    "Delete backwards with context-sensitive formatting."
    (interactive)
    (cond
     ((and (cb-hs:in-empty-braces?)
           (thing-at-point-looking-at (rx (+ space))))
      (delete-horizontal-space))
     (t
      (or (cb-op:delete-last-smart-op)
          (call-interactively 'sp-backward-delete-char)))))

  (defun cb-hs:smart-comma ()
    "Insert a comma, with context-sensitive formatting."
    (interactive)
    (cond
     ((ignore-errors (s-matches? "ExportSpec" (elt (shm-current-node) 0)))
      (delete-horizontal-space)
      (insert ",")
      (hi2-indent-line)
      (just-one-space))

     (t
      (insert ","))))

  (defun cb-hs:ghci-line-beginning-position ()
    "Narrow to the current line, excluding the ghci prompt."
    (save-excursion
      (cond ((haskell-interactive-at-prompt)
             (goto-char (line-beginning-position))
             (or (search-forward-regexp (s-trim-left haskell-interactive-prompt)
                                        (line-end-position)
                                        t)
                 (line-beginning-position)))
            (t
             (line-beginning-position)))))

  (defun cb-hs:ghci-smart-colon ()
    "Insert a smart operator, unless point is immediately after the GHCI prompt."
    (interactive)
    (save-restriction
      (narrow-to-region (cb-hs:ghci-line-beginning-position)
                        (line-end-position))
      (if (s-blank? (buffer-substring (line-beginning-position) (point)))
          (insert ":")
        (smart-insert-op ":"))))

  (defun cb-hs:ghci-smart-comma ()
    "Insert a comma with padding."
    (interactive)
    (save-restriction
      (narrow-to-region (cb-hs:ghci-line-beginning-position)
                        (point))
      (unless (s-blank? (current-line))
        (delete-horizontal-space))

      (insert ", ")))

  (declare-smart-ops 'haskell-mode
    :add '("$" "=")
    :custom
    '(("." . cb-hs:smart-dot)
      ("," . cb-hs:smart-comma)
      ("|" . cb-hs:smart-pipe)
      ("#" . cb-hs:smart-hash)
      (":" . cb-hs:smart-colon)))

  (define-key haskell-mode-map (kbd "SPC") 'cb-hs:space)
  (define-key haskell-mode-map (kbd "DEL") 'cb-hs:del)

  (declare-smart-ops 'haskell-interactive-mode
    :add '("$" "=")
    :custom
    '(("." . cb-hs:smart-dot)
      ("|" . cb-hs:smart-pipe)
      (":" . cb-hs:ghci-smart-colon)
      ("," . cb-hs:ghci-smart-comma)))

  (define-key shm-map (kbd "C-k")     'shm/kill-node)
  (define-key shm-map (kbd "C-c C-s") 'shm/case-split)
  (define-key shm-map (kbd "C-<return>") 'shm/newline-indent)

  (after 'evil

    (defun cb-hs:join-line ()
      (interactive)
      (forward-line 1)
      (goto-char (line-beginning-position))
      (call-interactively 'shm/delete-indentation))

    (evil-define-key 'normal shm-map "J" 'cb-hs:join-line))

  (define-key shm-map (kbd ",") nil)
  (define-key shm-map (kbd ":") nil)
  (define-key shm-map (kbd "#") nil)
  (define-key shm-map (kbd "-") nil)
  (define-key shm-map (kbd "DEL") nil)
  (define-key shm-map (kbd "SPC") nil)
  (define-key shm-map (kbd "<backtab>") nil)
  (define-key shm-map (kbd "TAB") nil)
  (define-key shm-map (kbd "M-r") nil)

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

  (after 'hideshow
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

  (cbs-define-search-method
   :name "hoogle"
   :key "h"
   :command
   (lambda (_)
     (call-interactively 'hoogle))
   :when
   (lambda ()
     (derived-mode-p 'haskell-mode 'haskell-interactive-mode)))

  (font-lock-add-keywords
   'haskell-mode
   `(("\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->"
      (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                ,(string-to-char "λ") 'decompose-region)
                nil)))))

  (define-key haskell-mode-map (kbd "C-,")     'haskell-move-nested-left)
  (define-key haskell-mode-map (kbd "C-.")     'haskell-move-nested-right)
  (define-key haskell-mode-map (kbd "C-c c")   'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-f") 'haskell-cabal-visit-file)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "M-q")     'haskell-mode-stylish-buffer)

  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

  (defvar cb-hs:language-pragmas
    (s-split "\n" (%-string "ghc --supported-languages"))
    "List the language pragmas available in GHC.")

  (defun cb-hs:language-pragmas-in-file ()
    "List the language pragmas set in the current file."
    (--filter (s-matches? it (buffer-string))
              cb-hs:language-pragmas))

  (defun cb-hs:available-language-pragmas ()
    "List the language pragmas that have not been set in the current file."
    (-difference cb-hs:language-pragmas (cb-hs:language-pragmas-in-file)))

  (defun cb-hs:insert-language-pragma (pragma)
    "Read a language pragma to be inserted at the start of this file."
    (interactive (list (ido-completing-read "Pragma: "
                                            (cb-hs:available-language-pragmas)
                                            nil t)))
    (let ((s (format "{-# LANGUAGE %s #-}\n" pragma)))
      (save-excursion
        (goto-char (point-min))
        (insert s))))

  (defun cb-hs:parse-module (str)
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

  (defun cb-hs:haskell-modules ()
    "Get a list of all Haskell modules known to the current project or GHC."
    (if (haskell-session-maybe)
        (haskell-session-all-modules)
      (->> (%-string "ghc-pkg" "dump")
        (s-split "---")
        (-mapcat 'cb-hs:parse-module)
        (-map 's-trim))))

  (defun cb-hs:do-insert-at-imports (str)
    "Prepend STR to this buffer's list of imported modules."
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
      (insert str)))

  (defun cb-hs:module->qualified-name (module)
    "Make a reasonable name for MODULE for use in a qualified import."
    (s-word-initials (-last-item (s-split (rx ".") module))))

  (defun cb-hs:insert-qualified-import (module name)
    "Interactively insert a qualified Haskell import statement for MODULE."
    (interactive
     (let ((m (s-trim (ido-completing-read "Module: " (cb-hs:haskell-modules)
                                           nil t))))
       (list m (s-trim (read-string "As: " (cb-hs:module->qualified-name m)
                                    t)))))

    (if (s-matches? (rx-to-string `(and "import" (+ space) "qualified" (+ space)
                                        ,module (or space eol)))
                    (buffer-string))
        (when (called-interactively-p nil)
          (message "Module '%s' is already imported" module))

      (cb-hs:do-insert-at-imports (format "import qualified %s as %s" module name))))

  (defun cb-hs:insert-import (module)
    "Interactively insert a Haskell import statement for MODULE."
    (interactive (list (ido-completing-read "Module: " (cb-hs:haskell-modules)
                                            nil t)))

    (if (s-matches? (rx-to-string `(and "import" (+ space) ,module (or space eol)))
                    (buffer-string))
        (when (called-interactively-p nil)
          (message "Module '%s' is already imported" module))

      (cb-hs:do-insert-at-imports (format "import %s" module))))

  (-each '(("i" "Haskell Import" cb-hs:insert-import :modes haskell-mode)
           ("q" "Haskell Qualified Import" cb-hs:insert-qualified-import :modes haskell-mode)
           ("l" "Haskell Language Extension" cb-hs:insert-language-pragma :modes haskell-mode))
    (~ add-to-list 'insertion-picker-options))

  (defun cb-hs:newline-and-insert-at-col (col str)
    "Insert STR on a new line at COL."
    (goto-char (line-end-position))
    (newline)
    (indent-to col)
    (insert str))

  (defun cb-hs:newline-indent-to-same-col ()
    "Make a new line below the current one and indent to the same column."
    (let ((col (save-excursion (back-to-indentation) (current-column))))
      (goto-char (line-end-position))
      (newline)
      (indent-to col)))

  (defvar cb-hs:haskell-keywords
    '("let" "where" "module" "case" "class" "data" "deriving" "default"
      "import" "infixl" "infixr" "newtype" "data" "type" "if" "then" "else"))

  (defun cb-hs:first-ident-on-line ()
    (car (-difference (s-split (rx space) (current-line) t)
                      cb-hs:haskell-keywords)))

  (defun cb-hs:first-ident-on-line ()
    (car (-difference (s-split (rx space) (current-line) t)
                      cb-hs:haskell-keywords)))

  (defun cb-hs:insert-function-template (fname)
    (back-to-indentation)

    (when (shm-current-node)
      (shm/goto-parent-end))

    (goto-char (line-end-position))
    (newline)
    (shm-insert-string (concat fname " ")))

  (defun cb-hs:at-decl-for-function? (fname)
    (when fname
      (or
       ;; A type decl exists in this buffer?
       (s-matches? (eval `(rx bol (* space)
                              (? (or "let" "where") (+ space))
                              ,fname (+ space) "::"))
                   (buffer-string))
       ;; At an equation?
       (s-matches? (eval `(rx bol (* space)
                              (? (or "let" "where") (+ space))
                              ,fname (+ nonl) "="))
                   (current-line)))))

  (defun cb-hs:start-col-of-string-on-line (str)
    "Return the column where STR starts on this line."
    (when str
      (save-excursion
        (goto-char (line-beginning-position))
        (search-forward str)
        (goto-char (match-beginning 0))
        (current-column))))

  (defun cb-hs:meta-ret ()
    "Open a new line in a context-sensitive way."
    (interactive)
    (yas-exit-all-snippets)
    (cond

     ;; Insert new case below the current type decl.
     ((s-matches? (rx bol (* space) "data") (current-line))
      (goto-char (line-end-position))
      (newline)
      (insert "| ")
      (goto-char (line-beginning-position))
      (hi2-indent-line)
      (goto-char (line-end-position))
      (message "New data case"))

     ;; Insert new type decl case below the current one.
     ((s-matches? (rx bol (* space) "|") (current-line))
      (cb-hs:newline-indent-to-same-col)
      (insert "| ")
      (message "New data case"))

     ;; Insert pattern match at function definition.
     ((s-matches? (rx bol (* space) (+ word) (+ space) "::") (current-line))
      (cb-hs:insert-function-template (cb-hs:first-ident-on-line))
      (message "New function case"))

     ;; Insert new pattern match case below the current one.
     ((or (s-matches? (rx bol (* space) (+ (not (any "="))) "->") (current-line))
          (s-matches? (rx bol (* space) "case" (+ space)) (current-line)))
      (cb-hs:newline-indent-to-same-col)
      (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name))
      (message "New pattern match case"))

     ;; Insert new line starting with comma.
     ((s-matches? (rx bol (* space) ",") (current-line))
      (cb-hs:newline-indent-to-same-col)
      (insert ", ")
      (message "New entry"))

     ;; Insert new line starting with an arrow.
     ((s-matches? (rx bol (* space) "->") (current-line))
      (cb-hs:newline-indent-to-same-col)
      (insert "-> ")
      (message "New arrow"))

     ;; Insert new line with a binding in do-notation.
     ((s-matches? (rx bol (* space) (+ nonl) "<-") (current-line))
      (back-to-indentation)
      (let ((col (current-column)))
        (search-forward "<-")
        (shm/forward-node)
        (newline)
        (indent-to col))

      (yas-insert-first-snippet (C (~ equal "do-binding") yas--template-name))
      (message "New do-binding"))

     ;; New function case.
     ((cb-hs:at-decl-for-function? (cb-hs:first-ident-on-line))
      (let* ((ident (cb-hs:first-ident-on-line))
             (col (cb-hs:start-col-of-string-on-line ident)))
        (cb-hs:insert-function-template ident)
        (save-excursion
          (back-to-indentation)
          (indent-to col)))
      (message "New binding case"))

     (t
      (goto-char (line-end-position))
      (hi2-newline-and-indent)
      (message "New line")))

    (when (true? evil-mode)
      (evil-insert-state)))

  (define-key haskell-mode-map (kbd "M-RET") 'cb-hs:meta-ret)

  (defun cb-hs:flyspell-verify ()
    "Prevent common flyspell false positives in haskell-mode."
    (and (flyspell-generic-progmode-verify)
         (not (or (s-matches? (rx bol (* space) "{-#") (current-line))
                  (s-matches? (rx bol (* space) "foreign import") (current-line))))))

  (hook-fn 'flyspell-prog-mode-hook
    (when (derived-mode-p 'haskell-mode)
      (setq-local flyspell-generic-check-word-predicate 'cb-hs:flyspell-verify)))

  (defun cb-hs:src-or-test-dir ()
    "Find the containing src or test directory."
    (-when-let (f-or-dir (or (buffer-file-name) default-directory))
      (cadr (s-match (rx (group (+ nonl) "/" (or "src" "test") "/")) f-or-dir))))

  (defun cb-hs:guess-session-dir (session)
    "Calculate a default directory for an interactive Haskell SESSION."
    (or (cb-hs:src-or-test-dir)
        (haskell-session-get session 'current-dir)
        (haskell-session-get session 'cabal-dir)
        (when (buffer-file-name) (f-dirname (buffer-file-name)))
        user-home-directory))

  (defun haskell-session-pwd (session &optional change)
    "Get the directory for SESSION.

When optional argument CHANGE is set, prompt for the current directory."
    (when (or change (null (haskell-session-get session 'current-dir)))
      (let* ((prompt (if change "Change directory: " "Set current directory: "))
             (default-dir (cb-hs:guess-session-dir session))
             (dir (haskell-utils-read-directory-name prompt default-dir)))
        (haskell-session-set-current-dir session dir)
        dir)))

  (defun cb:haskell-doc-current-info ()
    "Return the info about symbol at point.
Meant for `eldoc-documentation-function'."
    (-when-let (sig (haskell-doc-sym-doc (haskell-ident-at-point)))
      (with-temp-buffer
        ;; Initialise haskell-mode's font-locking.
        (set (make-local-variable 'font-lock-defaults)
             '(haskell-font-lock-choose-keywords
               nil nil ((?\' . "w") (?_  . "w")) nil
               (font-lock-syntactic-keywords
                . haskell-font-lock-choose-syntactic-keywords)
               (font-lock-syntactic-face-function
                . haskell-syntactic-face-function)
               ;; Get help from font-lock-syntactic-keywords.
               (parse-sexp-lookup-properties . t)))

        (insert sig)
        (font-lock-fontify-buffer)
        (buffer-string))))

  (hook-fn 'cb:haskell-modes-hook
    (setq-local eldoc-documentation-function 'cb:haskell-doc-current-info))

  )

(provide 'config-haskell)

;;; config-haskell.el ends here