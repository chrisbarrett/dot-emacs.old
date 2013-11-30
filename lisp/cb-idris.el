;;; cb-idris.el --- Configuration for the Idris language.

;; Copyright (C) 2013 Chris Barrett

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

;; Configuration for the Idris language.

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-mode-groups)
(autoload 'idris-indentation-indent-line "idris-indentation")
(autoload 'idris-newline-and-indent "idris-indentation")
(autoload 'sp-end-of-sexp "smartparens")
(autoload 'sp-get-sexp "smartparens")
(autoload 'thing-at-point "thingatpt")
(autoload 'thing-at-point-looking-at "thingatpt")
(autoload 'smart-insert-operator "smart-operator")

;; Configure smart operators for Idris

(defun cbidris:typing-operator-in-braces? ()
  (or (equal (char-before) ?\()
      (and (not (s-matches? (rx alnum) (char-to-string (char-before))))
           (equal (char-after) ?\)))))

(defun cbidris:smart-insert-operator (op)
  "Insert an operator with padding.
Does not pad if inside a pair of brackets.

* OP is the operator as a string."
  (let ((pt (point)))
    (cond
     ((cbidris:typing-operator-in-braces?)
      (delete-horizontal-space)
      (insert op))
     (t
      (smart-insert-operator op)))
    (save-excursion
      (goto-char pt)
      (idris-reformat-dwim t))))

(defun cbidris:inserting-cons-in-braces? ()
  (equal (char-after) ?\)))

(defun cbidris:smart-colon ()
  (interactive)
  (cond
   ((equal (string-to-char " ") (char-before))
    (smart-insert-operator ":"))
   ((cbidris:inserting-cons-in-braces?)
    (insert ":"))
   (t
    (smart-insert-operator ":")))

  (idris-reformat-dwim t))

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
  (if (s-matches? (rx "[" (* (any "|" alnum)) eol)
                  (buffer-substring (line-beginning-position) (point)))
      (insert "|")
    (cbidris:smart-insert-operator "|")))

(defun cbidris:looking-at-module-or-constructor? ()
  (-when-let (sym (thing-at-point 'symbol))
    (s-uppercase? (substring sym 0 1))))

(defun cbidris:smart-dot (&optional arg)
  "Insert a period. Add padding, unless this line is an import statement.
With a prefix arg, insert a period without padding."
  (interactive "*P")
  (cond
   (arg
    (insert "."))
   ((cbidris:looking-at-module-or-constructor?)
    (insert "."))
   ((thing-at-point-looking-at (rx (or "(" "{" "[") (* space)))
    (insert "."))
   (t
    (cbidris:smart-insert-operator "."))))

(defun cbidris:insert-arrow (arrow)
  "If point is inside a tuple or braces, insert an arrow inside.
Otherwise insert an arrow at the end of the line."
  (atomic-change-group
    (cl-destructuring-bind (&key beg end op &allow-other-keys)
        (sp-get-sexp t)
      ;; Check whether point is inside a tuple.
      (if (and (-contains? '("(" "{") op)
               (> (point) beg)
               (< (point) end))
          (sp-end-of-sexp)
        (end-of-line)))
    ;; Insert arrow.
    (just-one-space)
    (insert arrow)
    (just-one-space)))

(defun cbidris:at-typedecl? ()
  (s-matches? (rx space ":" (not (any ":")))
              (buffer-substring (line-beginning-position) (point))))

(defun cbidris:smart-minus (&optional arg)
  "Insert an arrow if we're in a typesig, otherwise perform a normal insertion.
With a prefix arg, insert an arrow with padding at point."
  (interactive "*P")
  (cond
   (arg
    (just-one-space)
    (insert "->")
    (just-one-space))
   ((cbidris:at-typedecl?)
    (cbidris:insert-arrow "->"))
   (t
    (cbidris:smart-insert-operator "-"))))

(defun cbidris:smart-lt (&optional arg)
  "Insert a less than symbol. With a prefix arg, insert an arrow at point."
  (interactive "*P")
  (cond
   (arg
    (just-one-space)
    (insert "<-")
    (just-one-space))
   (t
    (cbidris:smart-insert-operator "<"))))

(defun cbidris:smart-question-mark ()
  "Insert a ? char as an operator, unless point is after an = sign."
  (interactive)
  (cond
   ((s-matches? (rx "=" (* space) eol) (current-line))
    (just-one-space)
    (insert "?"))
   (t
    (cbidris:smart-insert-operator "?"))))

;; Set key bindings

(after 'idris-mode
  (define-keys idris-mode-map
    "," 'cbidris:smart-comma
    "&" (command (cbidris:smart-insert-operator "&"))
    "%" (command (cbidris:smart-insert-operator "%"))
    "?" 'cbidris:smart-question-mark
    "*" (command (cbidris:smart-insert-operator "*"))
    "+" (command (cbidris:smart-insert-operator "+"))
    "/" (command (cbidris:smart-insert-operator "/"))
    "-" 'cbidris:smart-minus
    "=" (command (cbidris:smart-insert-operator "="))
    "<" 'cbidris:smart-lt
    ">" (command (cbidris:smart-insert-operator ">"))
    "." 'cbidris:smart-dot
    ":" 'cbidris:smart-colon
    "|" 'cbidris:smart-pipe
    "$" (command (cbidris:smart-insert-operator "$"))))

;; Define code formatting commands for idris-mode.

;; -----------------------------------------------------------------------------
;; Datatypes

(defun cbidris:data-start-pos ()
  "Find the start position of the datatype declaration at point."
  (save-excursion
    (end-of-line)
    (search-backward-regexp (rx bol (* space) "data" eow) nil t)))

(defun cbidris:data-end-pos ()
  "Find the end position of the datatype declaration at point."
  (save-excursion
    (let ((start (point)))

      (goto-char (cbidris:data-start-pos))
      (forward-line)
      (goto-char (line-beginning-position))

      (let ((end
             (or
              (when (search-forward-regexp
                     (rx bol (or (and (* space) eol) (not (any space "|"))))
                     nil t)
                (forward-line -1)
                (line-end-position))

              (point-max))))
        (when (<= start end)
          end)))))

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

(cl-defun cbidris:goto-type-judgement-colon
    (&optional (bound (cbidris:data-end-pos)))
  (ignore-errors
    (goto-char (line-beginning-position))
    (search-forward " : " bound)
    (search-backward ":")))

(defun cbidris:max-colon-column-in-data ()
  "Find the greatest column of type judgements in a data decl."
  (->> (save-excursion
         (cl-loop
          while (and (not (eobp))
                     (cbidris:at-data-decl?))
          collect (progn
                    (cbidris:goto-type-judgement-colon)
                    (current-column))
          do (progn
               (forward-line)
               (end-of-line))))
    (-remove 'null)
    (cons 0)
    (-max)))

(defun cbidris:indent-data-decl ()
  "Indent the data decl at point."
  (when (< 1 (->> (cbidris:data-decl-at-pt)
               (s-split "\n")
               (length)))
    (let ((end (cbidris:data-end-pos)))
      (save-excursion

        (goto-char (cbidris:data-start-pos))
        (forward-line)

        (let (done)
          (while (and (not done) (<= (point) end))
            (goto-char (line-beginning-position))
            (delete-horizontal-space)
            (indent-for-tab-command)
            (if (save-excursion (goto-char (line-end-position))
                                (eobp))
                (setq done t)
              (forward-line))))))))

(defun cbidris:normalise-data-decl-colons ()
  (save-excursion
    (let ((start (cbidris:data-start-pos)))

      (goto-char start)
      (while (search-forward-regexp
              (rx space ":" (or space eol))
              (cbidris:data-end-pos) t)
        (save-excursion
          (search-backward ":")
          (just-one-space)))

      (goto-char start)
      (let ((col (cbidris:max-colon-column-in-data))
            done)
        (while (and (not done)
                    (cbidris:at-data-decl?))
          (when (cbidris:goto-type-judgement-colon)
            (indent-to col))
          (goto-char (line-end-position))
          (if (eobp)
              (setq done t)
            (forward-line)))))))

(defun cbidris:at-sum-type? ()
  "Non-nil if point is at a data decl for a sum type."
  (s-matches? (rx bol (* space) "data" (* nonl) "=")
              (cbidris:data-decl-at-pt)))

(defun cbidris:align-sum-cases ()
  "Align cases of a sum type with the '=' sign."
  (save-excursion
    ;; Ensure the '=' sign is surrounded by single spaces.
    (goto-char (cbidris:data-start-pos))
    (search-forward-regexp (rx space "=" (not (any "="))))
    (just-one-space)
    (search-backward "=")
    (just-one-space)
    (let ((col (current-column))
          done)
      (forward-line)
      (goto-char (line-end-position))
      (while (and (not done)
                  (cbidris:at-data-decl?))
        (when (s-matches? (rx bol (* space) "|") (current-line))
          (goto-char (line-beginning-position))
          (search-forward "|")
          (backward-char)
          (indent-to col))
        (goto-char (line-end-position))
        (if (eobp)
            (setq done t)
          (forward-line))))))

(defun cbidris:format-data-decl ()
  "Align colons in a datatype declaration."
  (when (cbidris:at-data-decl?)
    (cond
     ((cbidris:at-sum-type?)
      (cbidris:align-sum-cases))

     (t
      (cbidris:indent-data-decl)
      (cbidris:normalise-data-decl-colons)))

    t))

;; -----------------------------------------------------------------------------
;; Functions

(defun cbidris:after-function-args? ()
  "Non-nil if point is at a function application."
  (let ((fst-equals (s-index-of "=" (current-line))))
    (and (< fst-equals (current-column)))))

(defun cbidris:at-let-binding? ()
  "Non-nil if point is at a let-binding form."
  (s-matches? (rx bow "let" eow) (current-line)))

(defun cbidris:at-equation? ()
  "Non-nil if point is at a function definition or equation."
  (and (s-matches? (rx space "=" (or space eol)) (current-line))
       (not (cbidris:at-let-binding?))
       (not (cbidris:after-function-args?))))

(defun cbidris:function-case-lines (fname)
  "Return a list of lines for the function FNAME."
  (save-excursion
    (cl-loop
     initially (goto-char (point-min))
     while (search-forward-regexp (rx-to-string `(and bol (? "(") ,fname (? ")")))
                                  nil t)
     unless (s-matches? " : " (current-line))
     collect (line-number-at-pos))))

(defun cbidris:function-name-at-pt ()
  "Return the name of the function at point."
  (save-excursion
    (search-backward-regexp (rx bol (? "(")
                                (group (+ (not (any space ":" ")"))))
                                (? ")")))
    (let ((s (match-string-no-properties 1)))
      (unless (-contains? (cons "data" idris-keywords) s)
        s))))

(defun cbidris:pad-tokens (lines)
  "For each line in LINES, align tokens in columns by right-padding with whitespace."
  (cl-loop
   ;; Split lines into a matrix of argument expressions.
   for split-lines = (-map 's-split-sexps lines)
   ;; Pad with whitespace. This requires a matrix transposition before we can calculate
   ;; the width for each row.
   for padded =
   (cl-loop for col from 0 upto (-max (cons 0 (-map 'length split-lines)))
            for rows = (-map (~ nth col) split-lines)
            for widest = (-max (-map 'length rows))
            collect (-map (~ s-pad-right widest " ") rows))
   ;; Transpose matrix again to restore original ordering.
   for col from 0 upto (-max (cons 1 (-map 'length padded)))
   for rows = (-map (~ nth col) padded)
   collect rows))

(defun cbidris:columnate-arguments (lines)
  "Align function arguments by column for each line in LINE-NOS."
  (let* ((padded
          (->> (cbidris:pad-tokens lines)
            ;; Manually pad and align '=' sign, in case some equations are partial.
            (-map (C (~ s-chop-suffix "=") s-trim (~ s-join " ")))))
         (widest-arglist (-max (cons 0 (-map 'length padded)))))
    (->> padded
      (-remove (~ s-matches? (rx bol (* space) eol)))
      (-map (C (~ s-append "=") (~ s-pad-right widest-arglist " "))))))

(defun cbidris:bol-to-s (rx line-no)
  "Return the part of the line at LINUM from the line start up to RX."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-no))
    (buffer-substring-no-properties (line-beginning-position)
                                    (search-forward-regexp rx nil t))))

(defun cbidris:normalise-function-decl-arguments ()
  (let* ((linums (cbidris:function-case-lines (cbidris:function-name-at-pt)))
         (replacements (cbidris:columnate-arguments
                        (-map (~ cbidris:bol-to-s "=") linums))))
    (-each (-zip linums replacements)
           (lambda+ ((linum . s))
             (save-excursion
               (goto-char (point-min))
               (forward-line (1- linum))
               (search-forward-regexp (rx bol (group (+ (not (any "="))) "="))
                                      (line-end-position))
               (replace-match s t))))))

(defun cbidris:format-function-args ()
  "Align function declaration arguments."
  (when (cbidris:at-equation?)
    (cbidris:normalise-function-decl-arguments)
    t))

;; -----------------------------------------------------------------------------

(defun idris-reformat-dwim (&optional silent?)
  "Perform a context-sensitive reformatting command.
SILENT? controls whether provide feedback to the user on the action performed."
  (interactive "*")
  ;; HACK: Set point manually--something is moving point to the line start pos.
  (let ((pt (point)))
    (save-excursion
      (cond
       ((cbidris:format-data-decl)
        (unless silent?
          (message "Formatted data declaration.")))
       ((cbidris:format-function-args)
        (unless silent?
          (message "Formatted function arguments.")))
       (t
        (unless silent?
          (message "No context to reformat")))))
    (goto-char (max (point) pt))))

(defun idris-ret ()
  "Indent and align on newline."
  (interactive "*")
  (if (s-matches? comment-start (current-line))
      (comment-indent-new-line)
    (idris-reformat-dwim t)

    (if (s-matches? (rx bol (* space) eol) (current-line))
        (newline)
      (idris-newline-and-indent))))

(defun idris-meta-ret ()
  "Create a newline and perform a context-sensitive continuation.
* At functions, create a new case for the function.
* At types, add a 'where' statement if one does not exist.
* At comments, fill paragraph and insert a newline."
  (interactive)
  (cond
   ((cbidris:function-name-at-pt)
    (goto-char (line-end-position))
    (let ((fn (cbidris:function-name-at-pt)))

      (unless (s-matches? (rx bol (* space) eol) (current-line))
        (newline-and-indent))

      (insert fn)
      (just-one-space)))

   ((cbidris:at-data-decl?)
    (let ((dt (cbidris:data-decl-at-pt)))

      (unless (s-contains? "where" dt)
        (save-excursion
          (goto-char (cbidris:data-start-pos))
          (unless (s-matches? (rx space "=" (not (any "="))) (current-line))
            (goto-char (line-end-position))
            (just-one-space)
            (insert "where"))))

      (goto-char (line-end-position))
      (newline-and-indent)
      (indent-for-tab-command)))

   ((s-matches? comment-start (current-line))
    (fill-paragraph)
    (comment-indent-new-line))

   (t
    (idris-ret))))

(after 'idris-mode
  (define-keys idris-mode-map
    "M-q" 'idris-reformat-dwim
    "<return>" 'idris-ret
    "M-<return>" 'idris-meta-ret))

;; Configure Smartparens.
(after 'smartparens

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
    (sp-local-pair "'" nil :actions nil))
  (sp-with-modes cb:idris-modes
    (sp-local-pair "'" "'" :actions '(:rem insert))))

;; Use font lock to display Unicode symbols in Idris buffers.
(after 'idris-mode

  (defun cbidris:apply-font-lock (pat rep)
    "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
    (font-lock-add-keywords
     nil `((,pat
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(string-to-char rep) 'decompose-region)
                      nil))))))

  (defun cbidris:font-lock (patterns)
    (--each patterns
      (cl-destructuring-bind (pat rep) it
        (cbidris:apply-font-lock
         (rx-to-string `(and (not (any "\""))
                             (? "`")
                             (group  symbol-start ,pat symbol-end)
                             (? "`")
                             (not (any "\""))))
         rep))))

  (defun cbidris:apply-unicode ()
    (cbidris:apply-font-lock
     "\\s ?(?\\(\\\\\\)\\s *\\(\\w\\|_\\|(.*)\\).*?\\s *->" "λ")
    (cbidris:font-lock '(("<-"     "←")
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
                         ("!!"     "‼"))))

  (add-hook 'cb:idris-modes-hook 'cbidris:apply-unicode))

;; Configure eldoc for Idris.

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

;; `idris-mode' provides editing support for the Idris language.
(use-package idris-mode
  :mode (("\\.idr$" . idris-mode))
  :config
  (after 'idris-mode

    ;; HACK: redefine idris-mode to derive from prog-mode.
    (define-derived-mode idris-mode prog-mode "Idris"
      "Major mode for Idris
     \\{idris-mode-map}
Invokes `idris-mode-hook'."
      :syntax-table idris-syntax-table
      :group 'idris
      (set (make-local-variable 'font-lock-defaults)
           idris-font-lock-defaults)
      (set (make-local-variable 'indent-tabs-mode) nil)
      (set (make-local-variable 'comment-start) "--"))

    (define-key idris-mode-map (kbd "C-c C-z") 'idris-switch-to-output-buffer)))

;; Define a command to switch from the repl to the last Idris src buffer.

(defun idris-switch-to-src ()
  "Pop to the last idris source buffer."
  (interactive)
  (-if-let (buf (car (--filter-buffers (derived-mode-p 'idris-mode))))
      (pop-to-buffer buf)
    (error "No idris buffers")))

(after 'idris-repl
  (define-key idris-repl-mode-map (kbd "C-c C-z") 'idris-switch-to-src))

(provide 'cb-idris)

;;; cb-idris.el ends here
