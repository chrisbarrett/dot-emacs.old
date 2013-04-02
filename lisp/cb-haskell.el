;;; cb-haskell

;;; Testing

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
    (when (_ cb:hs-project-root)
      (cond
       ((file-exists-p file) (find-file file))
       (src-p (error "No corresponding unit test file found."))
       (t     (error "No corresponding source file found."))))))


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

(flycheck-declare-checker haskell-hdevtools
  "Haskell checker using hdevtools"
  :command '("hdevtools"
             "check"
             "-g" "-Wall"
             "-g" "-i/../src"
             source-inplace)
  :error-patterns cb:haskell-checker-regexes
  :modes 'haskell-mode)

(push 'haskell-hdevtools flycheck-checkers)

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

(font-lock-add-keywords 'haskell-mode
 `((,(concat "\\s (?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->")
    (0 (progn (compose-region (match-beginning 1) (match-end 1)
                              ,(make-char 'greek-iso8859-7 107))
              nil)))))

;;; Editing Advice

;; HACK: Use advice to override paredit comment dwim in Haskell mode.
;; It would be better to get this working with key bindings.
(defadvice paredit-comment-dwim (around haskell-override-comment-char activate)
  (if (derived-mode-p 'haskell-mode)
      (comment-dwim nil)
    ad-do-it))

;;; Modes

(defun cb:haskell-common-setup ()
  "Perform setup common to all Haskell-related modes."
  (add-to-list 'ac-sources my/ac-source-haskell)
  (yas-minor-mode nil)
  (haskell-doc-mode t)
  (scion-mode t)
  (cb:haskell-prettify-lambda)

  ;; Paredit setup. Override paredit defaults that are unsuitable for
  ;; editing Haskell.
  (ignore-errors (paredit-mode t))

  (local-set-key (kbd "C-c h") 'hoogle))

(defun cb:on-haskell-mode ()
  (cb:haskell-common-setup)
  (scion-mode t)

  ;; Outlining
  (setq outline-regexp haskell-outline-regex)
  (setq outline-level 'haskell-outline-level)
  (setq evil-shift-width 4)
  (outline-minor-mode t)

  (haskell-indentation-mode t)
  (setq haskell-tags-on-save t)
  (setq tab-width 4)

  (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build)
  (local-set-key (kbd "C-c h") 'hoogle)
  (local-set-key (kbd "C-c l") 'hs-lint)
  (local-set-key (kbd "C-c C-s") 'scion-load)
  (local-set-key (kbd "C-c j") 'haskell-test<->code))

(add-hook 'haskell-mode-hook 'cb:on-haskell-mode)
(add-hook 'inferior-haskell-hook 'cb:haskell-common-setup)
(add-hook 'haskell-cabal-mode-hook 'cb:on-haskell-mode)

(provide 'cb-haskell)
