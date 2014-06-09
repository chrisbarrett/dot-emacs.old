;;; config-idris.el --- Configure Idris

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

;; Configure Idris

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)

(cb:declare-package-installer idris
  :match (rx ".idr" eol)
  :packages (idris-mode))

(add-to-list 'completion-ignored-extensions ".ibc")

(custom-set-variables
 '(idris-warnings-printing 'warnings-repl))

(hook-fn 'cb:idris-modes-hook
  (setq-local tab-width 2)
  (setq-local evil-shift-width 2))

(add-hook 'idris-mode-hook 'idris-indentation-mode)
(after 'idris-indentation
  (diminish 'idris-indentation-mode))

(defadvice idris-mode (before start-process activate)
  (unless idris-process
    (idris-run)))

(after 'idris-mode

  (defun cb-idris:eldoc-fn ()
    (ignore-errors
      (noflet ((message (&rest _)))
        (-when-let* ((name (idris-name-at-point))
                     (str (car (idris-eval (list :type-of name)))))
          (cl-destructuring-bind (_ ident type)
              (s-match (rx (group (+ nonl)) ":" (group (+ nonl)) eol) str)
            (format "%s:%s"
                    (propertize ident 'face font-lock-function-name-face)
                    (propertize type 'face font-lock-type-face)))))))

  (hook-fn 'cb:idris-modes-hook
    (setq-local eldoc-documentation-function 'cb-idris:eldoc-fn)
    (turn-on-eldoc-mode))

  (define-key idris-mode-map (kbd "C-c C-z") 'idris-switch-to-output-buffer)

  (defadvice idris-switch-to-output-buffer (after evil-append activate)
    (cb:append-buffer))

  (defun cbidris:smart-colon ()
    (interactive)
    (cond
     ((and (char-before) (equal (char-to-string (char-before)) " "))
      (smart-op-insert ":"))
     ((and (char-after) (equal (char-to-string (char-after)) ")"))
      (insert ":"))
     (t
      (smart-op-insert ":"))))

  (defun cbidris:smart-comma ()
    (interactive)
    (cond
     ((s-matches? (rx bol (* space) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (insert ", ")
      (idris-indentation-indent-line))
     (t
      (insert ","))))

  (defun cbidris:smart-pipe ()
    "Insert a pipe operator. Add padding, unless we're inside a list."
    (interactive)
    (let ((in-empty-square-braces?
           (save-excursion
             (-when-let (pair (sp-backward-up-sexp))
               (cl-destructuring-bind (&key op beg end &allow-other-keys) pair
                 (and (equal "[" op)
                      (s-blank? (buffer-substring (1+ beg) (1- end)))))))))
      (cond
       (in-empty-square-braces?
        (delete-horizontal-space)
        (insert "| ")
        (save-excursion
          (insert " |"))
        (message "Inserting idiom brackets"))

       (t
        (smart-op-insert "|")))))


  (defun cbidris:smart-dot ()
    "Insert a period with context-sensitive padding."
    (interactive)
    (let ((looking-at-module-or-constructor?
           (-when-let (sym (thing-at-point 'symbol))
             (s-uppercase? (substring sym 0 1)))))
      (cond
       (looking-at-module-or-constructor?
        (insert "."))
       ((thing-at-point-looking-at (rx (or "(" "{" "[") (* space)))
        (insert "."))
       (t
        (smart-op-insert ".")))))

  (defun cbidris:smart-question-mark ()
    "Insert a ? char as an operator, unless point is after an = sign."
    (interactive)
    (cond
     ((s-matches? (rx "=" (* space) eol) (current-line))
      (just-one-space)
      (insert "?"))
     (t
      (smart-op-insert "?"))))

  (--each cb:idris-modes
    (declare-smart-ops it
      :add '("$")
      :custom
      '(("?" . cbidris:smart-question-mark)
        ("|" . cbidris:smart-pipe)
        ("." . cbidris:smart-dot)
        ("," . cbidris:smart-comma)
        (":" . cbidris:smart-colon))))

  (define-key idris-mode-map (kbd "<backspace>") 'sp-backward-delete-char)

  (defun cbidris:apply-unicode ()
    (font-lock-add-keywords
     nil `(("\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *=>"
            (0
             (progn (compose-region (match-beginning 1) (match-end 1)
                                    ?\λ 'decompose-region)
                    nil))))))

  (add-to-list 'font-lock-keywords-alist
               '(idris-mode
                 ((("^ *record\\>" . font-lock-keyword-face)))))

  (add-hook 'cb:idris-modes-hook 'cbidris:apply-unicode)

  (-each
      '((idris-semantic-type-face     . font-lock-type-face)
        (idris-semantic-data-face     . default)
        (idris-semantic-function-face . font-lock-function-name-face)
        (idris-semantic-bound-face    . font-lock-variable-name-face)
        (idris-semantic-implicit-face . font-lock-comment-face)
        (idris-repl-output-face       . compilation-info)
        )
    (~ add-to-list 'face-remapping-alist))

  (defun sp-idris-just-one-space (id action ctx)
    "Pad parens with spaces."
    (when (and (equal 'insert action)
               (sp-in-code-p id action ctx))
      ;; Insert a leading space, unless
      ;; 1. this is a quoted form
      ;; 2. this is the first position of another list
      ;; 3. this form begins a new line.
      (save-excursion
        (search-backward id)
        (unless (s-matches?
                 (rx (or (group bol (* space))
                         (any "," "`" "@" "(" "[" "{")) eol)
                 (buffer-substring (line-beginning-position) (point)))
          (just-one-space)))
      ;; Insert space after separator, unless
      ;; 1. this form is at the end of another list.
      ;; 2. this form is at the end of the line.
      (save-excursion
        (search-forward (sp-get-pair id :close))
        (unless (s-matches? (rx (or (any ")" "]" "}")
                                    eol))
                            (buffer-substring (point) (1+ (point))))
          (just-one-space)))))

  (sp-with-modes cb:idris-modes
    ;; Pad delimiters with spaces.
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-idris-just-one-space))
    (sp-local-pair "{" "}" :post-handlers '(:add sp-idris-just-one-space))
    (sp-local-pair "[" "]" :post-handlers '(:add sp-idris-just-one-space))
    (sp-local-pair "(" ")" :post-handlers '(:add sp-idris-just-one-space))
    (sp-local-pair "`" "`" :post-handlers '(:add sp-idris-just-one-space))
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "[|" "|]" :post-handlers '(:add sp-idris-just-one-space)))
  (sp-with-modes cb:idris-modes
    (sp-local-pair "'" "'" :actions '(:rem insert)))

  (defun cbidris:get-docstring ()
    "Format a docstring for eldoc."
    (ignore-errors
      (-when-let* ((name (car (idris-thing-at-point)))
                   (s (idris-eval `(:type-of ,name))))
        (nth 1 (s-match (rx (* (any "-" "\n" space)) (group (* anything)))
                        s)))))

  (defun cbidris:configure-eldoc ()
    "Set up eldoc for Idris."
    (setq-local eldoc-documentation-function 'cbidris:get-docstring)
    (eldoc-mode +1))

  (add-hook 'cb:idris-modes-hook 'cbidris:configure-eldoc)

  (defun idris-switch-to-src ()
    "Pop to the last idris source buffer."
    (interactive)
    (-if-let (buf (car (--filter-buffers (derived-mode-p 'idris-mode))))
        (pop-to-buffer buf)
      (error "No idris buffers")))

  (after 'idris-repl
    (define-key idris-repl-mode-map (kbd "C-c C-z") 'idris-switch-to-src))

  (defun cbidris:data-start-pos ()
    "Find the start position of the datatype declaration at point."
    (save-excursion
      (end-of-line)
      (when (search-backward-regexp (rx bol (* space) (or "record" "data") eow) nil t)
        (skip-chars-forward " \t")
        (point))))

  (defun cbidris:data-end-pos ()
    "Find the end position of the datatype declaration at point."
    (save-excursion
      (let ((start (point)))

        (goto-char (cbidris:data-start-pos))
        (forward-line)
        (goto-char (line-beginning-position))

        (let ((end
               (when (search-forward-regexp
                      (rx bol (or (and (* space) eol) (not (any space "|"))))
                      nil t)
                 (forward-line -1)
                 (line-end-position))))
          (if (and end (<= start end))
              end
            (point-max))))))

  (cl-defun cbidris:data-decl-at-pt ()
    "Return the data declaration at point."
    (-when-let* ((start (cbidris:data-start-pos))
                 (end (cbidris:data-end-pos)))
      (buffer-substring-no-properties start end)))

  (defun cbidris:at-data-decl? ()
    (-when-let (dd (cbidris:data-decl-at-pt))
      (let ((lines (s-split "\n" dd)))
        (or (equal 1 (length lines))
            (->> (-drop 1 lines)
              (-all? (~ s-matches? (rx bol (or space "|")))))))))

  (defun cbidris:function-name-at-pt ()
    "Return the name of the function at point."
    (save-excursion
      (search-backward-regexp (rx bol (* space) (group (+ (not (any space ":"))))))
      (let ((s (s-trim (match-string-no-properties 1))))
        (unless (or (-contains? idris-keywords s)
                    (s-blank? s))
          s))))

  (defun idris-ret ()
    "Indent and align on newline."
    (interactive "*")
    (if (s-matches? comment-start (current-line))
        (comment-indent-new-line)

      (cond

       ((s-matches? (rx space "->" (* space))
                    (buffer-substring (line-beginning-position) (point)))
        (newline)
        (delete-horizontal-space)
        (indent-for-tab-command))

       ((s-matches? (rx bol (* space) eol) (current-line))
        (delete-horizontal-space)
        (newline))

       (t
        (idris-newline-and-indent)))))

  (defun idris-meta-ret ()
    "Create a newline and perform a context-sensitive continuation.
- At functions, create a new case for the function.
- At types, add a 'where' statement if one does not exist.
- At comments, fill paragraph and insert a newline."
    (interactive)
    (cond

     ;; Insert new type decl case below the current one.
     ((s-matches? (rx bol (* space) "|") (current-line))
      (let ((col (save-excursion (back-to-indentation) (current-column))))
        (goto-char (line-end-position))
        (newline)
        (indent-to col))

      (insert "| ")
      (message "New data case"))

     ;; Insert new type decl case below the current one.
     ((and (s-matches? (rx bol (* space) "data") (current-line))
           (not (s-matches? "where" (current-line))))

      (-if-let (col (save-excursion
                      (goto-char (line-beginning-position))
                      (search-forward "=" nil t)
                      (current-column)))
          (progn
            (goto-char (line-end-position))
            (newline)
            (indent-to (1- col)))

        (goto-char (line-end-position))
        (idris-newline-and-indent))

      (insert "| ")
      (message "New data case"))

     ;; Create new function case.
     ((cbidris:function-name-at-pt)
      (goto-char (line-end-position))
      (let ((fn (cbidris:function-name-at-pt))
            (col (save-excursion
                   (back-to-indentation)
                   (current-column))))

        (unless (s-matches? (rx bol (* space) eol) (current-line))
          (newline))

        (indent-to-column col)
        (insert fn)
        (just-one-space)))

     ;; Insert new line starting with comma.
     ((s-matches? (rx bol (* space) ",") (current-line))
      (cb-hs:newline-indent-to-same-col)
      (insert ", ")
      (message "New entry"))

     ;; Create a new line in a comment.
     ((s-matches? comment-start (current-line))
      (fill-paragraph)
      (comment-indent-new-line)
      (message "New comment line"))

     (t
      (goto-char (line-end-position))
      (idris-ret)))

    (cb:maybe-evil-insert-state))

  (define-keys idris-mode-map
    "<return>" 'idris-ret
    "M-<return>" 'idris-meta-ret)

  (after 'evil
    (add-hook 'idris-info-mode-hook 'evil-emacs-state))

  )

(provide 'config-idris)

;;; config-idris.el ends here
