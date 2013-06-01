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
(autoload 'haskell-cabal-find-file "haskell-cabal")

(after 'haskell-mode

  (defun cb:hs-project-root ()
    (let ((cabal (haskell-cabal-find-file)))
      (when cabal (file-name-directory cabal))))

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

  ;; Use greek lambda symbol.
  (font-lock-add-keywords
   'haskell-mode
   `((,(concat "\\s (?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->")
      (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                ,(make-char 'greek-iso8859-7 107))
                nil))))))

(after 'flycheck

  (defun haskell-parse-phrase-for-err (phrase)
    (ignore-errors
      (destructuring-bind (_ file line col warn message)
          (s-match
           (rx (group-n 1 (+? nonl)) ":"  ; file
               (group-n 2 (+? digit)) ":" ; line
               (group-n 3 (+? digit)) ":" ; col
               (* (or "\n" space))
               (group-n 4 (? "Warning:") (* space))
               (group-n 5 (+ (or nonl "\n"))) ; message
               )
           phrase)
        (flycheck-error-new
         :line (string-to-number line)
         :column (string-to-number col)
         :level (if (s-blank? warn) 'error 'warning)
         :filename file
         :message (->> message
                    (s-split (rx "\n"))
                    (-map 's-trim)
                    (s-join "\n"))))))

  (defun haskell-ghc-parser (str &rest _)
    (->> str
      ;; Split on entirely blank lines.
      (s-split (rx bol eol))
      (-map 's-trim)
      (-map 'haskell-parse-phrase-for-err)
      (-remove 'null)))

  (flycheck-declare-checker haskell-ghc
    "Haskell checker using ghc"
    :command '("ghc" "-v0"
               "-Wall"
               "-i./../src"
               "-fno-code"
               source-inplace)
    :error-parser 'haskell-ghc-parser
    :modes 'haskell-mode
    :next-checkers '(haskell-hlint))

  (flycheck-declare-checker haskell-hlint
    "Haskell checker using hlint"
    :command '("hlint" source-inplace)
    :error-patterns
    `((,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): Error: "
                "\\(?4:\\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)")
       error)
      (,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): Warning: "
                "\\(?4:\\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)")
       warning))
    :modes 'haskell-mode)

  (add-to-list 'flycheck-checkers 'haskell-ghc)
  (add-to-list 'flycheck-checkers 'haskell-hlint)

  (hook-fn 'haskell-mode-hook
    (flycheck-select-checker 'haskell-ghc)))

(use-package haskell-mode
  :ensure t
  :commands
  (haskell-mode
   haskell-c-mode
   haskell-cabal-mode
   hoogle)
  :mode
  (("\\.hs$"    . haskell-mode)
   ("\\.hsc$"   . haskell-c-mode)
   ("\\.cabal$" . haskell-cabal-mode))
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
      (-when-let (buf (last-buffer-for-mode 'haskell-mode))
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

(use-package ghc
  :ensure   t
  :commands ghc-init
  :init     (add-hook 'haskell-mode-hook 'ghc-init)
  :config
  (progn

    ;; HACK: this command throws errors on haskell-mode startup.
    (defadvice ghc-uniq-lol (around ignore-errs activate)
      (ignore-errors ad-do-it))

    (defun ac-haskell-candidates ()
      "Auto-complete source using ghc-doc."
      (let ((pat (buffer-substring (ghc-completion-start-point) (point))))
        (all-completions pat (ghc-select-completion-symbol))))

    (ac-define-source ghc
      '((candidates . ac-haskell-candidates)))

    (hook-fn 'haskell-mode-hook
      (add-to-list 'ac-sources 'ac-source-ghc))))

(use-package haskell-ac
  :defer t
  :init
  (hook-fn 'cb:haskell-modes-hook
    (require 'auto-complete)
    (add-to-list 'ac-modes major-mode)
    (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
    (add-to-list 'ac-sources 'ac-source-haskell)))

(use-package haskell-edit
  :commands (haskell-find-type-signature
             haskell-reformat-type-signature))

(use-package haskell-indentation
  :diminish haskell-indentation-mode
  :defer    t
  :init     (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

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

(provide 'cb-haskell)

;;; cb-haskell.el ends here
