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

(autoload 'haskell-cabal-find-file "haskell-cabal")
(require 's)

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

;;; Checkers

(defconst cb:haskell-checker-regexes
     `((,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]*\\):[ \t\n\r]*"
                 "\\(?4:Warning: \\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)") warning)

       (,(concat "^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]*\\):[ \t\n\r]*"
                 "\\(?4:\\(.\\|[ \t\n\r]\\)+?\\)\\(^\n\\|\\'\\)") error)))

(flycheck-declare-checker haskell-ghc
  "Haskell checker using ghc"
  :command '("ghc" "-v0"
             "-Wall"
             "-i./../src"
             "-fno-code"
             source-inplace)
  :error-patterns cb:haskell-checker-regexes
  :modes 'haskell-mode)

(push 'haskell-ghc flycheck-checkers)

;; (flycheck-declare-checker haskell-hdevtools
;;   "Haskell checker using hdevtools"
;;   :command '("hdevtools"
;;              "check"
;;              "-g" "-Wall"
;;              "-g" "-i/../src"
;;              source-inplace)
;;   :error-patterns cb:haskell-checker-regexes
;;   :modes 'haskell-mode)

;; (push 'haskell-hdevtools flycheck-checkers)

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

;;; Cosmetic

(defconst cb:haskell-outline-regex "[^\t ].*\\|^.*[\t ]+\\(where\\|of\\|do\\|in\\|if\\|then\\|else\\|let\\|module\\|import\\|deriving\\|instance\\|class\\)[\t\n ]")

(defun cb:hs-outline-level ()
  "Use spacing to determine outlining."
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

;;; Use greek lambda symbol.
(font-lock-add-keywords 'haskell-mode
 `((,(concat "\\s (?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->")
    (0 (progn (compose-region (match-beginning 1) (match-end 1)
                              ,(make-char 'greek-iso8859-7 107))
              nil)))))

;;; Editing Advice

(defadvice paredit-comment-dwim (around haskell-override-comment-char activate)
  "Override paredit-dwim so that haskell comment chars are used."
  (if (derived-mode-p 'haskell-mode)
      (comment-dwim nil)
    ad-do-it))

(provide 'cb-haskell)

;;; cb-haskell.el ends here
