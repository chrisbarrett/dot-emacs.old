(require 'utils-common)
(require 'utils-ui)

(defvar-local smart-op-list
  '("=" "<" ">" "%" "+" "-" "*" "/" "&" "|" "!" ":")
  "A list of strings to treat as operators.")

(defun in-string? ()
  "Non-nil if point is inside a string according to font locking."
  (-contains? '(font-lock-string-face
                font-lock-doc-face
                font-lock-doc-string-face)
              (face-at-point)))

(defun in-comment? ()
  "Non-nil if point is inside a comment according to font locking."
  (ignore-errors
    (equal 'font-lock-comment-face (face-at-point))))

(defun cb-op:prev-non-space-char ()
  "Return the previous non-whitespace character on this line, as a string."
  (save-excursion
    (when (search-backward-regexp (rx (not space))
                                  (line-beginning-position) t)
      (thing-at-point 'char))))

(defun cb-op:delete-horizontal-space-non-readonly ()
  "Delete horizontal space around point that is not read-only."
  (while (and (not (eobp))
              (s-matches? (rx space) (char-to-string (char-after)))
              (not (get-char-property (point) 'read-only)))
    (forward-char 1))

  (while (and (not (bobp))
              (s-matches? (rx space) (char-to-string (char-before)))
              (not (get-char-property (1- (point)) 'read-only)))
    (delete-char -1)))

(defun cb-op:maybe-just-one-space-after-operator ()
  "Insert a trailing space unless:
- the next char is an operator
- we are in a parenthesised operator."
  (unless (or (and (not (eolp))
                   (-contains? smart-op-list (char-to-string (char-after))))
              (thing-at-point-looking-at
               (eval `(rx "(" (+ (or ,@smart-op-list)) ")"))))
    (just-one-space)))

(defun smart-insert-op (op)
  "Insert a smart operator, unless we're in a string or comment."

  ;; Narrow to the current active snippet field if yasnippet is active. This
  ;; prevents errors when attempting to delete whitespace outside the current
  ;; field.
  (yas-with-field-restriction

    (cond
     ((or (in-string?) (in-comment?)
          ;; Looking at quotation mark?
          (-contains? '(?\" ?\') (char-after)))
      (insert op))

     ((-contains? (cl-list* "(" smart-op-list) (cb-op:prev-non-space-char))
      (cb-op:delete-horizontal-space-non-readonly)
      (insert op)
      (cb-op:maybe-just-one-space-after-operator))

     (t
      (unless (s-matches? (rx bol (* space) eol)
                          (buffer-substring (line-beginning-position) (point)))
        (just-one-space))

      (insert op)
      (cb-op:maybe-just-one-space-after-operator)))))

(defmacro make-smart-op (str)
  "Return a function that will insert smart operator STR.
Useful for setting up keymaps manually."
  (let ((fname (intern (concat "smart-insert-op/" str))))
    `(progn
       (defun ,fname ()
         "Auto-generated command.  Inserts a smart operator."
         (interactive "*")
         (smart-insert-op ,str))
       ',fname)))

(defun cb-op:add-smart-ops (ops custom)
  (let ((custom-ops (-map 'car custom)))
    (setq-local smart-op-list (-union ops custom-ops))
    (--each ops
      (local-set-key (kbd it) (eval `(make-smart-op ,it))))
    (--each custom
      (cl-destructuring-bind (op . fn) it
        (local-set-key (kbd op) fn)))))

(cl-defun declare-smart-ops (mode &key add rem custom)
  "Define the smart operators for the given mode.

- MODE is the mode to add the smart ops for.

- ADD is a list of smart operators to add to the defaults.

- REM is a list of smart operators to remove from the defaults.

- CUSTOM is a list of special operator insertion commands to use
  instead of the defaults. It is an alist of (OP . FUNCTION),
  where OP is a string and FUNCTION is a symbol."
  (declare (indent 1))
  (cl-assert (symbolp mode))
  (cl-assert (null (-intersection add rem)))
  (cl-assert (null (-intersection add (-map 'car custom))))
  (cl-assert (null (-intersection rem (-map 'car custom))))

  (let ((hook (intern (concat (symbol-name mode) "-hook")))
        (ops (-union (-map 'car custom)
                     (-difference (-union smart-op-list add) rem))))

    ;; Set smart ops list for buffers that already exist.
    (--each (--filter-buffers (derived-mode-p mode))
      (with-current-buffer it
        (cb-op:add-smart-ops ops custom)))
    ;; Set smart ops in mode's hook.
    (add-hook hook `(lambda ()
                      (cb-op:add-smart-ops ',ops ',custom)))

    (list :mode mode :ops ops)))

(defun cb-op:delete-last-smart-op ()
  "Delete the last smart-operator that was inserted."
  (unless (or (derived-mode-p 'text-mode) (in-string?) (in-comment?))
    (save-restriction
      (narrow-to-region (line-beginning-position) (point))

      (when (s-matches? (concat (regexp-opt smart-op-list) " *$")
                        (buffer-substring (line-beginning-position) (point)))
        ;; Delete op
        (let ((op-pos
               (save-excursion
                 (search-backward-regexp (regexp-opt smart-op-list)))))
          (while (and (/= (point) op-pos)
                      (not (get-char-property (point) 'read-only)))
            (delete-char -1)))

        ;; Delete preceding spaces.
        (cb-op:delete-horizontal-space-non-readonly)
        t))))

(defadvice sp-backward-delete-char (around delete-smart-op activate)
  "Delete the smart operator that was just inserted, including padding."
  (or (cb-op:delete-last-smart-op) ad-do-it))

(defadvice smart-insert-op (around restrict-to-insert-state activate)
  "If evil mode is active, only insert in insert state."
  (cond
   ((and (true? evil-mode) (evil-insert-state-p))
    ad-do-it)
   ((true? evil-mode))
   (t
    ad-do-it)))

(defun turn-on-linum-mode ()
  (linum-mode +1))

(add-hook 'prog-mode-hook 'turn-on-linum-mode)
(add-hook 'nxml-mode-hook 'turn-on-linum-mode)
(add-hook 'sgml-mode-hook 'turn-on-linum-mode)

(require 'lambda-mode)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

(add-hook 'cb:scheme-modes-hook    'lambda-mode)
(add-hook 'inferior-lisp-mode-hook 'lambda-mode)
(add-hook 'lisp-mode-hook          'lambda-mode)
(add-hook 'cb:elisp-modes-hook     'lambda-mode)
(add-hook 'cb:python-modes-hook    'lambda-mode)
(add-hook 'cb:slime-modes-hook     'lambda-mode)

(hook-fn 'lambda-mode-hook
  (diminish 'lambda-mode))

(after 'asm-mode

(put 'asm-mode 'tab-width 8)

(declare-smart-ops 'asm-mode
  :rem '("%" "-" "."))

(defun cb:asm-toggling-tab ()
  (interactive)
  (if (equal (line-beginning-position)
             (progn (back-to-indentation) (point)))
      (indent-for-tab-command)
    (indent-to-left-margin)))

(defun cb:asm-tab ()
  "Perform a context-sensitive indentation."
  (interactive)
  (if (s-contains? ":" (thing-at-point 'line))
      (indent-to-left-margin)
    (cb:asm-toggling-tab)))

(define-key asm-mode-map (kbd "<tab>") 'cb:asm-tab)

(defun cb:asm-electric-colon ()
  "Insert a colon, indent, then newline."
  (interactive)
  (atomic-change-group
    (unless (thing-at-point-looking-at (rx ":" (* space) eol))
      (insert ":"))
    (cb:asm-tab)
    (newline-and-indent)))

(define-key asm-mode-map (kbd ":") 'cb:asm-electric-colon)

)

(cb:declare-package-installer json
  :match "\\.json"
  :packages (json-mode))

(after 'json-mode
  (define-key json-mode-map (kbd "M-q") 'json-mode-beautify))

(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

(setq csv-align-style 'auto)
(add-hook 'csv-mode-hook 'csv-align-fields)

(defvar-local cb-csv:aligned? nil)

(defadvice csv-align-fields (after set-aligned activate)
  (setq cb-csv:aligned? t))

(defadvice csv-unalign-fields (after set-unaligned activate)
  (setq cb-csv:aligned? nil))

(defun cb-csv:toggle-field-alignment ()
  "Toggle field alignment in the current CSV buffer."
  (interactive)
  (call-interactively
   (if cb-csv:aligned? 'csv-unalign-fields 'csv-align-fields)))

(after 'csv-mode
  (define-key csv-mode-map (kbd "C-c C-t") 'cb-csv:toggle-field-alignment))

(hook-fn 'find-file-hook
  (when (s-starts-with? "<?xml " (buffer-string))
    (nxml-mode)))

(defun tidy-xml-buffer ()
  "Reformat the current XML buffer using Tidy."
  (interactive)
  (save-excursion
    (call-process-region (point-min) (point-max) "tidy" t t nil
                         "-xml" "-i" "-wrap" "0" "-omit" "-q")))

(after 'nxml-mode
  (define-key nxml-mode-map (kbd "M-q") 'tidy-xml-buffer))

(setq-default sgml-xml-mode t)

(after 'sgml-mode
  (define-key sgml-mode-map (kbd "M-q") 'tidy-xml-buffer))

(cb:declare-package-installer markdown
  :match (rx "." (or "md" "markdown") eol)
  :packages (markdown-mode))

(add-to-list 'auto-mode-alist
             `(,(rx "." (or "md" "markdown") eol) . markdown-mode))

(after 'markdown-mode

(put 'markdown-mode 'imenu-generic-expression
     '(("title"  "^\\(.*\\)[\n]=+$" 1)
       ("h2-"    "^\\(.*\\)[\n]-+$" 1)
       ("h1"   "^# \\(.*\\)$" 1)
       ("h2"   "^## \\(.*\\)$" 1)
       ("h3"   "^### \\(.*\\)$" 1)
       ("h4"   "^#### \\(.*\\)$" 1)
       ("h5"   "^##### \\(.*\\)$" 1)
       ("h6"   "^###### \\(.*\\)$" 1)
       ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

(after 'smartparens
  (sp-with-modes '(markdown-mode)
    (sp-local-pair "```" "```")))

(set-face-attribute markdown-header-face-1 nil :height 1.3)
(set-face-attribute markdown-header-face-2 nil :height 1.1)

(after 'evil
  (evil-define-key 'normal markdown-mode-map
    (kbd "M-P") 'outline-previous-visible-heading
    (kbd "M-N") 'outline-next-visible-heading))

)

(cb:declare-package-installer c-languages
  :match (rx "." (or "c" "cc" "cpp" "h" "hh" "hpp" "m") eol)
  :packages
  (ac-c-headers
   google-c-style
   c-eldoc
   clang-format))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(when (executable-find "clang")
  (setq cc-compilers-list (list "clang")
        cc-default-compiler "clang"
        cc-default-compiler-options "-fno-color-diagnostics -g"))

(after 'cc-mode
  (require 'google-c-style)

(define-key c-mode-map (kbd "M-q")
  (if (executable-find "clang-format")
      'clang-format-region
    'indent-dwim))

(after 'emr

  (defun helm-insert-c-header ()
    (interactive)
    (helm :sources
          `((name . "C Headers")
            (candidates . ,(-concat emr-c:standard-headers
                                    (emr-c:headers-in-project)))
            (action .
                    (lambda (c)
                      (emr-c-insert-include
                       (format (if (-contains? emr-c:standard-headers c)
                                   "<%s>"
                                 "\"%s\"")
                               c))
                      (when (derived-mode-p 'bison-mode)
                        (bison-format-buffer))))
            (volatile))
          :prompt "Header: "
          :buffer "*Helm C Headers*"))

  (add-to-list 'insertion-picker-options
               '("i" "Header Include" helm-insert-c-header
                 :modes (c-mode c++-mode))))

(defun cb-c:switch-between-header-and-impl ()
  "Switch between a header file and its implementation."
  (interactive)
  (let* ((ext   (if (f-ext? (buffer-file-name) "h") "c" "h"))
         (counterpart (format "%s.%s" (f-no-ext (buffer-file-name)) ext)))
    (if (or (f-file? counterpart)
            (y-or-n-p (format "%s does not exist.  Create it?" counterpart)))
        (find-file counterpart)
      (message "Aborted"))))

(define-key c-mode-map (kbd "C-c C-a") 'cb-c:switch-between-header-and-impl)

(defun cb-c:looking-at-flow-control-header? ()
  (thing-at-point-looking-at
   (rx (* nonl) (? ";") (* space)
       (or "if" "when" "while" "for")
       (* nonl)
       "("
       (* (not (any ")"))))))

(defun cb-c:looking-at-flow-control-keyword? ()
  (thing-at-point-looking-at
   (rx (or (group (or "if" "when" "while" "for") (or (+ space) "("))
           (group (or "do" "else") (* space))))))

(defun cb-c:looking-at-assignment-right-side? ()
  (save-excursion
    (thing-at-point-looking-at
     (rx "=" (* space)
         ;; Optional casts
         (? (group "(" (* nonl) ")"))
         (* space)))))

(defun cb-c:looking-at-cast? ()
  (let ((cast (rx

               (or
                "return"
                (any
                 ;; Operator
                 "+" "-" "*" "/" "|" "&" ">" "<"
                 ;; Expression delimiter
                 ";" "[" "{" "(" ")" "="))

               (* space)

               ;; Cast and type
               "(" (* nonl) ")"

               (* space)))
        )
    (and (thing-at-point-looking-at cast)
         (save-excursion
           (search-backward-regexp cast)
           (not (cb-c:looking-at-flow-control-keyword?))))))

(defun cb-c:looking-at-struct-keyword? ()
  (save-excursion
    (beginning-of-sexp)
    (thing-at-point-looking-at (rx (or "{" " " "(" ",") "."))))

(cl-defun cb-c:header-guard-var (&optional (header-file (buffer-file-name)))
  "Return the variable to use in a header guard for HEADER-FILE."
  (format "_%s_H_" (s-upcase (f-filename (f-no-ext header-file)))))

(defun cb-c:maybe-remove-spaces-after-insertion (pred-regex op-start-regex)
  (when (thing-at-point-looking-at pred-regex)
    (save-excursion
      (let ((back-limit (save-excursion
                          (search-backward-regexp op-start-regex)
                          (point))))
        (while (search-backward-regexp (rx space) back-limit t)
          (delete-horizontal-space)))
      (indent-according-to-mode))))

(defun cb-c:just-one-space-after-semicolon ()
  (save-excursion
    (when (search-backward-regexp (rx ";" (* space)) (line-beginning-position) t)
      (replace-match "; " nil))))

(defun c-insert-smart-equals ()
  "Insert an '=' with context-sensitive formatting."
  (interactive)
  (if (or (cb-c:looking-at-flow-control-header?)
          (cb-c:looking-at-struct-keyword?))
      (insert "=")
    (smart-insert-op "=")))

(defun c-insert-smart-star ()
  "Insert a * with padding in multiplication contexts."
  (interactive)
  (cond
   ((s-matches? (rx bol (* space) eol)
                (buffer-substring (line-beginning-position) (point)))
    (indent-according-to-mode)
    (insert "*"))
   ((thing-at-point-looking-at (rx (any "(" "{" "[") (* space)))
    (insert "*"))
   ((thing-at-point-looking-at (rx (any digit "*") (* space)))
    (smart-insert-op "*"))
   (t
    (just-one-space)
    (insert "*"))))

(defun c-insert-smart-minus ()
  "Insert a minus with padding unless a unary minus is more appropriate."
  (interactive)
  (atomic-change-group
    ;; Handle formatting for unary minus.
    (if (thing-at-point-looking-at
         (rx (or "return" "," "(" "[" "(" ";" "=") (* space)))
        (insert "-")
      (smart-insert-op "-"))
    ;; Collapse whitespace for decrement operator.
    (cb-c:maybe-remove-spaces-after-insertion
     (rx "-" (* space) "-" (* space))
     (rx (not (any "-" space))))
    (cb-c:just-one-space-after-semicolon)))

(defun c-insert-smart-gt ()
  "Insert a > symbol with formatting.
If the insertion creates an right arrow (->), remove surrounding whitespace.
If the insertion creates a <>, move the cursor inside."
  (interactive)
  (smart-insert-op ">")
  (cb-c:maybe-remove-spaces-after-insertion
   (rx (or "-" "<") (* space) ">" (* space))
   (rx (not (any space "<" "-" ">"))))
  (when (thing-at-point-looking-at "<>")
    (forward-char -1)))

(defun c-insert-smart-plus ()
  "Insert a + symbol with formatting.
Remove horizontal whitespace if the insertion results in a ++."
  (interactive)
  (smart-insert-op "+")
  (cb-c:maybe-remove-spaces-after-insertion
   (rx "+" (* space) "+" (* space))
   (rx (not (any space "+"))))
  (cb-c:just-one-space-after-semicolon))

(declare-smart-ops 'c-mode
  :add '("?")
  :custom
  '(("," . cb:comma-then-space)
    ("=" . c-insert-smart-equals)
    ("+" . c-insert-smart-plus)
    (">" . c-insert-smart-gt)
    ("-" . c-insert-smart-minus)
    ("*" . c-insert-smart-star)))

(defun cb-c:format-after-brace (_id action contexxt)
  "Apply formatting after a brace insertion."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               ;; Search backward for flow control keywords.
               (search-backward "{")
               (or (thing-at-point-looking-at
                    (rx symbol-start (or "else" "do")))
                   (progn
                     (sp-previous-sexp)
                     (thing-at-point-looking-at
                      (rx symbol-start (or "if" "for" "while")))))))
    ;; Insert a space for padding.
    (save-excursion
      (search-backward "{")
      (just-one-space))
    ;; Put braces on new line.
    (newline)
    (save-excursion (newline-and-indent))
    (c-indent-line)))

(defun cb-c:format-after-paren (_id action context)
  "Insert a space after flow control keywords."
  (when (and (equal action 'insert)
             (equal context 'code)
             (save-excursion
               (search-backward "(")
               (thing-at-point-looking-at
                (rx symbol-start (or "=" "return" "if" "while" "for")
                    (* space)))))
    (save-excursion
      (search-backward "(")
      (just-one-space))))

(after 'smartparens
  (sp-with-modes '(c-mode cc-mode c++-mode)
    (sp-local-pair "{" "}" :post-handlers '(:add cb-c:format-after-brace))
    (sp-local-pair "(" ")" :post-handlers '(:add cb-c:format-after-paren))))

(defun cbclang:flyspell-verify ()
  (not (s-matches? (rx bol (* space) "#include ") (current-line))))

(hook-fns '(c-mode-hook c++-mode-hook)
  (setq-local flyspell-generic-check-word-predicate 'cbclang:flyspell-verify))

(defadvice c-inside-bracelist-p (around ignore-errors activate)
  (ignore-errors ad-do-it))

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(add-hook 'c-mode-hook 'flyspell-mode-off)

(defun cb-cc:between-empty-braces-same-line? ()
  (and (s-matches? (rx "{" (* space) eol)
                   (buffer-substring (line-beginning-position) (point)))
       (s-matches? (rx bol (* space) "}")
                   (buffer-substring (point) (line-end-position)))))

(defun cb-cc:newline-and-indent ()
  "Insert newlines, performing context-specific formatting.

When point is between braces, insert an empty line between them so that

{|}

becomes

{
  |
}"
  (interactive)
  (when (cb-cc:between-empty-braces-same-line?)
    (delete-horizontal-space)

    (save-excursion
      (search-backward "{")
      (unless (thing-at-point-looking-at (rx bol (* space) "{"))
        (newline-and-indent)))

    (save-excursion
      (newline-and-indent)))
  (call-interactively 'newline-and-indent))


(defun cb-cc:delete-brace-contents ()
  (cl-destructuring-bind (&optional &key beg end op &allow-other-keys)
      (sp-get-enclosing-sexp)
    (when (equal op "{")
      (delete-region (1+ beg) (1- end))
      (goto-char (1+ beg)))))

(defun cb-cc:between-empty-braces-any-lines? ()
  (cl-destructuring-bind (&optional &key beg end op &allow-other-keys)
      (sp-get-enclosing-sexp)
    (when (equal op "{")
      (s-matches? (rx bos (* (any space "\n")) eos)
                  (buffer-substring (1+ beg) (1- end))))))

(defun cb-cc:backward-delete-char ()
  "Delete backwards, performing context-specific formatting.

When point is between empty braces over any number of lines, collapse them:

{
  |
}

becomes

{ | }

then

{|}"
  (interactive)
  (cond
   ((and (equal (char-before) ?{)
         (equal (char-after) ?}))
    (sp-backward-delete-char))

   ((and (thing-at-point-looking-at (rx "{" (+ space) "}"))
         (cb-cc:between-empty-braces-same-line?))
    (delete-horizontal-space))

   ((cb-cc:between-empty-braces-any-lines?)
    (cb-cc:delete-brace-contents)
    (insert "  ")
    (forward-char -1))

   (t
    (sp-backward-delete-char))))

(--each (list c-mode-map c++-mode-map java-mode-map objc-mode-map)
  (define-key it (kbd "RET") 'cb-cc:newline-and-indent)
  (define-key it (kbd "<backspace>") 'cb-cc:backward-delete-char))

)

(cb:declare-package-installer rust
  :match (rx ".rs" eol)
  :packages (rust-mode))

(declare-smart-ops 'rust-mode
  :rem '("!" "~" "&"))

(defun cbrs:insert-type-brackets ()
  (interactive)
  (save-restriction
    (narrow-to-region (line-beginning-position) (point))
    (delete-horizontal-space)
    (insert "<>")
    (forward-char -1)))

(after 'rust-mode
  (define-key rust-mode-map (kbd "C-c <") 'cbrs:insert-type-brackets))

(put 'rust :flycheck-command
     '("rustc" "--crate-type" "lib" "--no-trans"
       (option-list "-L" flycheck-rust-library-path s-prepend)
       source-inplace))

(autoload 'bison-mode "bison-mode")
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))

(defun cb-bison:m-ret ()
  "Perform a context-sensitive newline action."
  (interactive)
  (cond
   ;; First case after production identifier
   ((s-matches? (rx ":" (* space) eol) (current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "  "))
   ;; Second case
   ((save-excursion
      (forward-line -1)
      (s-matches? (rx ":" (* space) eol) (current-line)))
    (goto-char (line-end-position))
    (newline)
    (insert "| "))
   ;; New case
   ((s-matches? (rx bol (* space) "|") (current-line))
    (goto-char (line-end-position))
    (newline)
    (insert "| "))
   ;; Otherwise open a new line.
   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (when (true? evil-mode)
    (evil-insert-state))

  (bison-format-buffer))

(after 'bison-mode
  (define-key bison-mode-map (kbd "M-RET") 'cb-bison:m-ret))

(after 'bison-mode
  (define-key bison-mode-map (kbd "=") (make-smart-op "=")))

(hook-fn 'cb:lisp-modes-hook
  (local-set-key (kbd "M-q") 'indent-dwim))

(require 'smartparens)
(let ((ls (assoc 'interactive sp-navigate-reindent-after-up)))
  (setcdr ls (-uniq (-concat (cdr ls) cb:lisp-modes))))

(defun cblisp:just-inserted-double-quotes? (id action ctx)
  (and (sp-in-string-p id action ctx)
       (s-matches? (rx (not (any "\\")) "\"" eol)
                   (buffer-substring (line-beginning-position) (point)))))

(defun sp-lisp-just-one-space (id action ctx)
  "Pad LISP delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (cblisp:just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is a quoted form
    ;; 2. this is the first position of another list
    ;; 3. this form begins a new line.
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "," "`" "'" "@" "#" "~" "(" "[" "{")
                       ;; HACK: nREPL prompt
                       (and (any alnum "." "/" "-") ">" (* space)))
                   eol)
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

(sp-with-modes cb:lisp-modes
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "{" "}" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "[" "]" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "(" ")" :post-handlers '(:add sp-lisp-just-one-space))
  (sp-local-pair "'" nil :actions nil))

(cb:install-package 'parenface)
;; Work around issue loading parenface.
(require 'parenface
         (f-join
          (--first (s-matches? "parenface" it) (f-directories package-user-dir))
          "parenface.el"))

(cb:install-package 'eval-sexp-fu t)
(add-hook 'cb:lisp-modes-hook 'turn-on-eval-sexp-fu-flash-mode)
(setq eval-sexp-fu-flash-duration 0.2)

(add-to-list 'face-remapping-alist '(eval-sexp-fu-flash . intense-flash))

(add-hook 'cb:lisp-modes-hook 'turn-on-eldoc-mode)

(hook-fn 'eldoc-mode-hook
  (diminish 'eldoc-mode))

(cb:install-package 'redshank)

(add-hook 'cb:lisp-modes-hook 'turn-on-redshank-mode)

(hook-fn 'redshank-mode-hook
  (diminish 'redshank-mode))

(hook-fn 'common-lisp-mode-hook
  (cb:install-package 'slime t))

(setq slime-lisp-implementations `((lisp ("sbcl" "--noinform"))))

(defun run-slime ()
  "Run slime, prompting for a lisp implementation."
  (interactive)
  (let ((current-prefix-arg '-))
    (slime)))

(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Carton" . emacs-lisp-mode))

(setq-default flycheck-emacs-lisp-load-path (list cb:lib-dir "./"))

(defun cb:special-elisp-buffer? ()
  (and (derived-mode-p 'emacs-lisp-mode)
       (or
        (true? scratch-buffer)
        (s-ends-with? "-steps.el" (buffer-name))
        (s-matches? (rx bol (? (any "*" "."))
                        (or "org-"
                            "Org "
                            "Cask"
                            "Carton"
                            "scratch"
                            "emacs-lisp"
                            "autoloads"
                            (group "-pkg.el")
                            (group "Pp" (* anything) "Output")
                            "dir-locals"))
                    (buffer-name)))))

(defun cbel:configure-flycheck ()
  (when (and (derived-mode-p 'emacs-lisp-mode)
             (or (cb:special-elisp-buffer?) (buffer-narrowed-p)))
    (ignore-errors
      (flycheck-select-checker 'emacs-lisp))))

(add-hook 'flycheck-mode-hook 'cbel:configure-flycheck)

(sp-local-pair (-difference cb:lisp-modes cb:elisp-modes)
               "`" "`" :when '(sp-in-string-p))

(cbs-define-search-method
 :name "Apropos"
 :key "a"
 :command
 (lambda (_)
   (call-interactively 'helm-apropos))
 :when
 (lambda ()
   (apply 'derived-mode-p cb:elisp-modes)))

(defun cbel:find-identifier-prefix ()
    "Find the commonest identifier prefix in use in this buffer."
    (let ((ns-separators (rx (or ":" "--" "/"))))
      (->> (buffer-string)
        ;; Extract the identifiers from declarations.
        (s-match-strings-all
         (rx bol (* space)
             "(" (? "cl-") (or "defun" "defmacro" "defvar" "defconst")
             (+ space)
             (group (+ (not space)))))
        ;; Find the commonest prefix.
        (-map 'cadr)
        (-filter (~ s-matches? ns-separators))
        (-map (C car (~ s-match (rx (group (* nonl) (or ":" "--" "/"))))))
        (-group-by 'identity)
        (-max-by (-on '>= 'length))
        (car))))

  (defun cbel:find-group-for-snippet ()
    "Find the first group defined in the current file,
falling back to the file name sans extension."
    (or
     (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not
     space))))
                    (buffer-string)))
     (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                    (buffer-string)))
     (f-no-ext (f-filename buffer-file-name))))

  (define-obsolete-function-alias 'cbel:bol-for-snippet? 'cbyas:bol?)

  (defun cbel:simplify-arglist (text)
    "Return a simplified docstring of arglist TEXT."
    (->> (ignore-errors
           (read (format "(%s)" text)))
      (--keep
       (ignore-errors
         (cond
          ((listp it)
           (-first (& symbolp (C (N (~ s-starts-with? "&")) symbol-name))
                   it))
          ((symbolp it) it))))
      (-remove (C (~ s-starts-with? "&") symbol-name))))

  (defun cbel:cl-arglist? (text)
    "Non-nil if TEXT is a Common Lisp arglist."
    (let ((al (ignore-errors (read (format "(%s)" text)))))
      (or (-any? 'listp al)
          (-intersection al '(&key &allow-other-keys &body)))))

  (defun cbel:defun-form-for-arglist (text)
    "Return either 'defun or 'cl-defun depending on whether TEXT
is a Common Lisp arglist."
    (if (cbel:cl-arglist? text) 'cl-defun 'defun))

  (defun cbel:defmacro-form-for-arglist (text)
    "Return either 'defmacro or 'cl-defmacro depending on whether TEXT
is a Common Lisp arglist."
    (if (cbel:cl-arglist? text) 'cl-defmacro 'defmacro))

  (defun cbel:process-docstring (text)
    "Format a function docstring for a snippet.
TEXT is the content of the docstring."
    (let ((docs (->> (cbel:simplify-arglist text)
                  (-map (C s-upcase symbol-name))
                  (s-join "\n\n"))))
      (unless (s-blank? docs)
        (concat "\n\n" docs))))

(hook-fn 'minibuffer-setup-hook
  (when (equal this-command 'eval-expression)
    (paredit-mode +1)))

(cb:install-package 'elisp-slime-nav)

(hook-fn 'cb:elisp-modes-hook
  (elisp-slime-nav-mode +1)
  (local-set-key (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

  ;; Make M-. work in normal state.
  (after 'evil
    (evil-local-set-key 'normal (kbd "M-.")
                        'elisp-slime-nav-find-elisp-thing-at-point)))

(hook-fn 'elisp-slime-nav-mode-hook
  (diminish 'elisp-slime-nav-mode))

(cb:install-package 'cl-lib-highlight)
(hook-fn 'emacs-lisp-mode
  (cl-lib-highlight-initialize)
  (cl-lib-highlight-warn-cl-initialize))

(after 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-t") 'ert)
  (define-key emacs-lisp-mode-map (kbd "C-c C-l")
    'emacs-lisp-byte-compile-and-load))

(after 'ielm
  (define-keys ielm-map
    "M-RET" 'newline-and-indent
    "C-j" 'newline-and-indent))

(defun switch-to-ielm ()
  "Start up or switch to an Inferior Emacs Lisp buffer."
  (interactive)
  ;; HACK: rebind switch-to-buffer so ielm opens in another window.
  (noflet ((switch-to-buffer (buf) (switch-to-buffer-other-window buf)))
    (ielm)
    (cb:append-buffer)))

(defun switch-to-elisp ()
  "Switch to the last active elisp buffer."
  (interactive)
  (-when-let (buf (--first-buffer (derived-mode-p 'emacs-lisp-mode)))
    (switch-to-buffer-other-window buf)))

(after 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'switch-to-ielm))
(after 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'switch-to-elisp))

(defun send-to-ielm ()
  "Send the sexp at point to IELM"
  (interactive)
  (sp-kill-sexp nil 'yank)
  (unwind-protect
      (progn (switch-to-ielm)
             (delete-region (save-excursion
                              (search-backward-regexp (rx bol "ELISP>"))
                              (search-forward "> ")
                              (point))
                            (line-end-position))
             (yank))
    (setq kill-ring (cdr kill-ring))))

(defun eval-in-ielm ()
  "Eval the sexp at point in ielm."
  (interactive)
  (send-to-ielm)
  (ielm-return)
  (recenter -1)
  (switch-to-elisp))

(after 'lisp-mode
  (define-keys emacs-lisp-mode-map
    "C-c C-e" 'send-to-ielm
    "C-c RET" 'eval-in-ielm))

(after 'evil
  (define-evil-doc-handler cb:elisp-modes
    (let ((sym (symbol-at-point)))
      (cond
       ((symbol-function sym)
        (describe-function sym))
       ((and (boundp sym) (not (facep sym)))
        (describe-variable sym))
       ((facep sym)
        (describe-face sym))
       (t
        (user-error "No documentation available"))))))

(add-hook 'ielm-mode-hook 'smartparens-strict-mode)

(after 'hideshow
  (add-to-list 'hs-special-modes-alist
               '(inferior-emacs-lisp-mode "(" ")" ";.*$" nil nil)))

(add-hook 'ielm-mode-hook 'hs-minor-mode)

(put 'ielm-mode 'comment-start ";")

(defconst cb-el:let-expression-re
  (regexp-opt '("(let" "(-if-let*" "(-when-let*"))
  "Regex matching the start of a let expression.")

(defun cb-el:let-expr-start ()
  "Move to the start of a let expression."
  (cl-flet ((at-let? () (thing-at-point-looking-at cb-el:let-expression-re)))
    (while (and (sp-backward-up-sexp) (not (at-let?))))
    (when (at-let?) (point))))

(defun cb-el:at-let-binding-form? ()
  "Non-nil if point is at the top of a binding form in a let expression."
  (and (save-excursion (cb-el:let-expr-start))
       (save-excursion
         (sp-backward-up-sexp 3)
         (thing-at-point-looking-at cb-el:let-expression-re))))
(defun cb-el:M-RET ()
  "Perform context-sensitive newline behaviour."
  (interactive)
  (cond
   ;; Insert let-binding
   ((save-excursion (cb-el:let-expr-start))
    (until (cb-el:at-let-binding-form?) (sp-backward-up-sexp))
    (sp-up-sexp)
    (newline-and-indent)
    (insert "()")
    (forward-char -1))
   (t
    (sp-up-sexp)
    (newline-and-indent)
    (when (true? evil-mode)
      (evil-insert-state)))))

(define-key emacs-lisp-mode-map (kbd "M-RET") 'cb-el:M-RET)

(after 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-f") 'eval-buffer)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region))

(defadvice eval-region (after region-evaluated-message activate)
  (when (called-interactively-p nil)
    (message "Region evaluated.")))

(defadvice eval-buffer (after buffer-evaluated-feedback activate)
  (when (called-interactively-p nil)
    (message "Buffer evaluated.")))

(defun cbel:after-save ()
  (check-parens)
  (unless no-byte-compile
    (byte-compile-file (buffer-file-name))))

(hook-fn 'emacs-lisp-mode-hook
  (when (cb:special-elisp-buffer?) (setq-local no-byte-compile t))
  (add-hook 'after-save-hook 'cbel:after-save nil t))

(dash-enable-font-lock)

(--each cb:elisp-modes
  (font-lock-add-keywords
   it
   `(
     ;; General keywords
     (,(rx "(" (group (or "cl-destructuring-bind"
                          "cl-case")
                      symbol-end))
      (1 font-lock-keyword-face))
     ;; Macros and functions
     (,(rx bol (* space) "("
           (group-n 1 (or "cl-defun" "cl-defmacro"
                          "cl-defstruct"
                          "cl-defsubst"
                          "cl-deftype"))
           (+ space)
           (group-n 2 (+? anything) symbol-end))
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)))))

(--each cb:elisp-modes
  (font-lock-add-keywords
   it
   `(
     ;; General keywords
     (,(rx "(" (group (or "until"
                          "hook-fn"
                          "hook-fns"
                          "lambda+"
                          "after"
                          "noflet"
                          "ert-deftest"
                          "ac-define-source"
                          "evil-global-set-keys"
                          "flycheck-declare-checker"
                          "flycheck-define-checker")
                      symbol-end))
      (1 font-lock-keyword-face))

     ;; definition forms
     (,(rx bol (* space) "("
           (group-n 1
                    symbol-start
                    (* (not space))
                    (or "declare" "define" "extend" "gentest")
                    (+ (not space))
                    symbol-end)
           (+ space)
           (group-n 2 (+ (regex "\[^ )\n\]"))
                    symbol-end))
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)))))

(cb:declare-package-installer clojure
  :match (rx "." (or "clj" "edn" "dtm" "cljs" "cljx"))
  :packages (clojure-mode
             cider
             ac-nrepl))

(after 'evil
  (define-evil-doc-handler cb:clojure-modes (call-interactively 'cider-doc)))

(setq cider-popup-stacktraces    nil
      nrepl-hide-special-buffers t)

(defun cb:switch-to-cider ()
  "Start cider or switch to an existing cider buffer."
  (interactive)
  (-if-let (buf (get-buffer "*cider*"))
      (cider-switch-to-repl-buffer buf)
    (cider-jack-in)))

(after 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c C-z") 'cb:switch-to-cider))

(after 'cider

(define-key clojure-mode-map (kbd "C-c C-h") 'cider-doc)

(defadvice cider-popup-buffer-display (after set-mode activate)
  (with-current-buffer (ad-get-arg 0)
    (help-mode)))

(defun cb:switch-to-clojure ()
  "Switch to the last active clojure buffer."
  (interactive)
  (-when-let (buf (--first-buffer (derived-mode-p 'clojure-mode)))
    (pop-to-buffer buf)))

(define-key cider-repl-mode-map (kbd "C-c C-z") 'cb:switch-to-clojure)

(after 'cider-interaction

  (defun cider-emit-doc-into-popup-buffer (buffer value)
    "Emit into BUFFER the provided VALUE."
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (goto-char (point-max))
        (insert (format "%s" value))
        (indent-sexp)
        (font-lock-fontify-buffer)
        (goto-char (point-min)))))

  (defun cider-doc--handler (buffer)
    "Make a handler for evaluating and printing stdout/stderr in popup BUFFER."
    (nrepl-make-response-handler buffer
                                 '()
                                 (lambda (buffer str)
                                   (cider-emit-doc-into-popup-buffer buffer str))
                                 (lambda (buffer str)
                                   (cider-emit-doc-into-popup-buffer buffer str))
                                 '()))

  (defun cider-doc-handler (symbol)
    "Create a handler to lookup documentation for SYMBOL."
    (let ((form (format "(clojure.repl/doc %s)" symbol))
          (doc-buffer (cider-popup-buffer cider-doc-buffer t)))
      (cider-tooling-eval form
                          (cider-doc--handler doc-buffer)
                          nrepl-buffer-ns))))

(hook-fns '(clojure-mode-hook cider-repl-mode-hook)
  (cider-turn-on-eldoc-mode))

(-each (--filter-buffers (derived-mode-p 'clojure-mode))
       'cider-turn-on-eldoc-mode)

(defun cb:eval-last-clj-buffer ()
  "Evaluate that last active clojure buffer without leaving the repl."
  (interactive)
  (-when-let (buf (--first-buffer (derived-mode-p 'clojure-mode)))
    (with-current-buffer buf
      (cider-eval-buffer))))

(define-key clojure-mode-map (kbd "C-c C-f") 'cider-eval-buffer)
(define-key cider-repl-mode-map (kbd "C-c C-f") 'cb:eval-last-clj-buffer)

(set-face-attribute 'cider-error-highlight-face t :inherit 'error)
(set-face-underline 'cider-error-highlight-face nil)

(add-hook 'cider-repl-mode-hook 'cb:maybe-evil-insert-state)

(defadvice cider-switch-to-repl-buffer (after insert-at-end-of-cider-line activate)
  (cb:maybe-evil-insert-state))

(defadvice back-to-indentation (around move-to-cider-bol activate)
  "Move to position after prompt in cider."
  (if (equal major-mode 'cider-mode)
      (nrepl-bol)
    ad-do-it))

(define-key cider-repl-mode-map (kbd "C-l") 'cider-repl-clear-buffer)

)

(defun cbclj:pad-for-arglist (text)
  "Pad TEXT for insertion into an arglist after existing parameters."
  (unless (s-blank? text)
    (s-prepend " " (s-trim-left text))))

(defun cbclj:ns-for-current-buf ()
  "Calculate the namespace to use for the current buffer."
  (if (buffer-file-name)
      (s-replace "/" "."
                 (if (s-matches? "src" (buffer-file-name))
                     (->> (buffer-file-name)
                       f-no-ext
                       (s-split "src/")
                       -last-item)
                   (f-no-ext (f-filename (buffer-file-name)))))
    "name"))

(defun cb:stop-overtone ()
  "Stop synthesis."
  (interactive)
  (cider-eval "(stop)" nil)
  (message "Synthesis stopped."))

(defun overtone-doc-handler (symbol)
  "Create a handler to lookup documentation for SYMBOL."
  (let ((form (format "(odoc %s)" symbol))
        (doc-buffer (cider-popup-buffer cider-doc-buffer t)))
    (cider-tooling-eval form
                        (cider-popup-eval-out-handler doc-buffer)
                        nrepl-buffer-ns)))

(defun overtone-doc (query)
  "Open a window with the docstring for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'overtone-doc-handler query))

(defalias 'odoc 'overtone-doc)

(defun cbot:overtone-project-reference-p ()
  "Non-nil if the project.clj imports overtone."
  (-when-let (clj (and (projectile-project-p)
                       (f-join (projectile-project-root) "project.clj")))
    (when (f-exists? clj)
      (s-contains? "overtone" (f-read-text clj)))))

(defvar overtone-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-g") 'cb:stop-overtone)
    (define-key km (kbd "s-.") 'cb:stop-overtone)
    (define-key km (kbd "C-c C-h") 'odoc)
    km))

(define-minor-mode overtone-mode
  "Provide additional overtone-related functionality for clojure."
  nil " overtone" overtone-mode-map)

(defun maybe-enable-overtone-mode ()
  "Enable `overtone-mode' only if the current buffer or project references overtone."
  (when (and (not overtone-mode)
             (derived-mode-p 'clojure-mode 'cider-repl-mode)
             (cbot:overtone-project-reference-p))
    (overtone-mode t)))

(define-globalized-minor-mode global-overtone-mode overtone-mode
  maybe-enable-overtone-mode)

(add-hook 'clojure-mode-hook 'global-overtone-mode)

(after 'evil
  (define-evil-doc-handler cb:scheme-modes
    (call-interactively 'geiser-doc-symbol-at-point)))

(hook-fn 'cb:scheme-modes-hook
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(after 'scheme
  (cb:install-package 'geiser))

(after 'geiser

(setq geiser-mode-start-repl-p t
      geiser-repl-startup-time 20000
      geiser-repl-history-filename (f-join cb:tmp-dir "geiser-history")
      geiser-active-implementations '(racket))

(defun geiser-eval-buffer (&optional and-go raw nomsg)
  "Eval the current buffer in the Geiser REPL.

With prefix, goes to the REPL buffer afterwards (as
`geiser-eval-buffer-and-go')"
  (interactive "P")
  (let ((start (progn
                 (goto-char (point-min))
                 (while (s-matches? (rx bol "#") (current-line))
                   (forward-line))
                 (point)))
        (end (point-max)))
    (save-restriction
      (narrow-to-region start end)
      (check-parens))
    (geiser-debug--send-region nil
                               start
                               end
                               (and and-go 'geiser--go-to-repl)
                               (not raw)
                               nomsg)))

(define-key geiser-mode-map (kbd "C-c C-f") 'geiser-eval-buffer)

(define-key geiser-mode-map (kbd "C-c C-h") 'geiser-doc-look-up-manual)
(define-key geiser-repl-mode-map (kbd "C-c C-h") 'geiser-doc-look-up-manual)

(defadvice switch-to-geiser (after append-with-evil activate)
  (when (derived-mode-p 'comint-mode)
    (goto-char (point-max))))

(after 'evil
  (evil-define-key 'normal geiser-mode-map
    (kbd "M-.") 'geiser-edit-symbol-at-point))

(after 'evil
  (defadvice switch-to-geiser (after append-with-evil activate)
    (when (derived-mode-p 'comint-mode)
      (cb:maybe-evil-insert-state))))

)

(defconst cbscm:scm-buf "*execute scheme*")

(defun cbscm:lang (s)
  (or (cadr (s-match (rx bol "#lang" (+ space) (group (+ nonl))) s))
      "racket"))

(defun cbscm:run-file (file language)
  (interactive "f")
  (start-process cbscm:scm-buf cbscm:scm-buf
                 "racket" "-I" language file))

(defun cbscm:execute-buffer ()
  "Compile and run the current buffer in Racket."
  (interactive)
  ;; Kill running processes and prepare buffer.
  (with-current-buffer (get-buffer-create cbscm:scm-buf)
    (read-only-mode +1)
    (ignore-errors (kill-process))
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))))

  ;; Start a new Scheme process in the appropriate language for this file.
  (let ((lang (cbscm:lang (buffer-string))))
    (cond
     ;; Create a temp file if there are unwritten changes or this buffer does
     ;; not have a corresponding file.
     ((or (buffer-modified-p)
          (and (buffer-file-name) (not (f-exists? (buffer-file-name)))))
      (let ((f (make-temp-file nil nil ".rkt")))
        (f-write (buffer-string) 'utf-8 f)
        (cbscm:run-file f lang)))
     ;; Otherwise run this file directly.
     (t
      (cbscm:run-file (buffer-file-name) lang))))

  (display-buffer-other-frame cbscm:scm-buf))

(after 'scheme
  (define-key scheme-mode-map (kbd "C-c C-c") 'cbscm:execute-buffer))

(after 'scheme
  (put 'begin                 'scheme-indent-function 0)
  (put 'begin-for-syntax      'scheme-indent-function 0)
  (put 'case                  'scheme-indent-function 1)
  (put 'cond                  'scheme-indent-function 0)
  (put 'delay                 'scheme-indent-function 0)
  (put 'do                    'scheme-indent-function 2)
  (put 'lambda                'scheme-indent-function 1)
  (put 'λ                     'scheme-indent-function 1)
  (put 'lambda:               'scheme-indent-function 1)
  (put 'case-lambda           'scheme-indent-function 0)
  (put 'lambda/kw             'scheme-indent-function 1)
  (put 'define/kw             'scheme-indent-function 'defun)
  (put 'let                   'scheme-indent-function 'scheme-let-indent)
  (put 'let*                  'scheme-indent-function 1)
  (put 'letrec                'scheme-indent-function 1)
  (put 'let-values            'scheme-indent-function 1)
  (put 'let*-values           'scheme-indent-function 1)
  (put 'fluid-let             'scheme-indent-function 1)
  (put 'let/cc                'scheme-indent-function 1)
  (put 'let/ec                'scheme-indent-function 1)
  (put 'let-id-macro          'scheme-indent-function 2)
  (put 'let-macro             'scheme-indent-function 2)
  (put 'letmacro              'scheme-indent-function 1)
  (put 'letsubst              'scheme-indent-function 1)
  (put 'sequence              'scheme-indent-function 0) ; SICP, not r4rs
  (put 'letsyntax             'scheme-indent-function 1)
  (put 'let-syntax            'scheme-indent-function 1)
  (put 'letrec-syntax         'scheme-indent-function 1)
  (put 'syntax-rules          'scheme-indent-function 1)
  (put 'syntax-id-rules       'scheme-indent-function 1)

  (put 'call-with-input-file  'scheme-indent-function 1)
  (put 'call-with-input-file* 'scheme-indent-function 1)
  (put 'with-input-from-file  'scheme-indent-function 1)
  (put 'with-input-from-port  'scheme-indent-function 1)
  (put 'call-with-output-file 'scheme-indent-function 1)
  (put 'call-with-output-file* 'scheme-indent-function 1)
  (put 'with-output-to-file   'scheme-indent-function 'defun)
  (put 'with-output-to-port   'scheme-indent-function 1)
  (put 'with-slots            'scheme-indent-function 2)
  (put 'with-accessors        'scheme-indent-function 2)
  (put 'call-with-values      'scheme-indent-function 2)
  (put 'dynamic-wind          'scheme-indent-function 'defun)

  (put 'if                    'scheme-indent-function 1)
  (put 'method                'scheme-indent-function 1)
  (put 'beforemethod          'scheme-indent-function 1)
  (put 'aftermethod           'scheme-indent-function 1)
  (put 'aroundmethod          'scheme-indent-function 1)
  (put 'when                  'scheme-indent-function 1)
  (put 'unless                'scheme-indent-function 1)
  (put 'thunk                 'scheme-indent-function 0)
  (put 'while                 'scheme-indent-function 1)
  (put 'until                 'scheme-indent-function 1)
  (put 'parameterize          'scheme-indent-function 1)
  (put 'parameterize*         'scheme-indent-function 1)
  (put 'syntax-parameterize   'scheme-indent-function 1)
  (put 'with-handlers         'scheme-indent-function 1)
  (put 'with-handlers*        'scheme-indent-function 1)
  (put 'begin0                'scheme-indent-function 1)
  (put 'with-output-to-string 'scheme-indent-function 0)
  (put 'ignore-errors         'scheme-indent-function 0)
  (put 'no-errors             'scheme-indent-function 0)
  (put 'matcher               'scheme-indent-function 1)
  (put 'match                 'scheme-indent-function 1)
  (put 'regexp-case           'scheme-indent-function 1)
  (put 'dotimes               'scheme-indent-function 1)
  (put 'dolist                'scheme-indent-function 1)

  (put 'with-syntax           'scheme-indent-function 1)
  (put 'syntax-case           'scheme-indent-function 2)
  (put 'syntax-case*          'scheme-indent-function 3)
  (put 'syntax-parse          'scheme-indent-function 1)
  (put 'module                'scheme-indent-function 2)

  (put 'syntax                'scheme-indent-function 0)
  (put 'quasisyntax           'scheme-indent-function 0)
  (put 'syntax/loc            'scheme-indent-function 1)
  (put 'quasisyntax/loc       'scheme-indent-function 1)

  (put 'cases                 'scheme-indent-function 1)

  (put 'for                   'scheme-indent-function 1)
  (put 'for*                  'scheme-indent-function 1)
  (put 'for/list              'scheme-indent-function 1)
  (put 'for*/list             'scheme-indent-function 1)
  (put 'for/fold              'scheme-indent-function 2)
  (put 'for*/fold             'scheme-indent-function 2)
  (put 'for/and               'scheme-indent-function 1)
  (put 'for*/and              'scheme-indent-function 1)
  (put 'for/or                'scheme-indent-function 1)
  (put 'for*/or               'scheme-indent-function 1)

  (put 'nest                  'scheme-indent-function 1))

(after 'scheme
  (--each cb:scheme-modes
    (font-lock-add-keywords
     it
     `(;; Special forms in Typed Racket.
       (,(rx "("
             (group (or
                     ;; let family
                     (and (? "p") "let" (* (syntax word)) ":")
                     (and "let/" (+ (syntax word)))
                     ;; lambdas
                     (and (* (syntax word)) "lambda:")
                     ;; loops
                     (and "for" (* (syntax word)) ":")
                     "do:"
                     ;; Types
                     "struct:"
                     ":"
                     "provide:"
                     "cast"))

             eow)
        (1 font-lock-keyword-face))

       ;; Definition forms
       (,(rx "(" (group "def" (* (syntax word)) eow))
        (1 font-lock-keyword-face))

       ;; Bindings created by `define-values'
       (,(rx "(define-values" (+ space)
             "(" (group (+ (or (syntax word) space))) ")")
        (1 font-lock-variable-name-face))

       ;; General binding identifiers
       (,(rx "(def" (* (syntax word)) (+ space)
             (group (+ (syntax word))))
        (1 font-lock-variable-name-face))

       ;; Function identifiers
       (,(rx "(def" (* (syntax word)) (+ space)
             "(" (group (+ (syntax word))))
        (1 font-lock-function-name-face))

       ;; Function identifier in type declaration
       (,(rx "(:" (+ space) (group bow (+ (syntax word)) eow))
        (1 font-lock-function-name-face))

       ;; Arrows
       (,(rx bow "->" eow)
        (0 (prog1 nil (compose-region (match-beginning 0) (match-end 0) "→"))))

       ;; Match keywords
       (,(rx "(" (group "match" (* (syntax word)) eow))
        (1 font-lock-keyword-face))

       ;; Error signalling keywords
       (,(rx "(" (group (or "error" "raise")
                        (* (syntax word)) eow))
        (1 font-lock-warning-face))

       ;; Grab-bag of keywords
       (,(rx "(" (group (or (and "begin" num)
                            "parameterize"
                            ))
             eow)
        (1 font-lock-keyword-face))))

    ;; Do not add type font locking to the REPL, because it has too many false
    ;; positives.
    (font-lock-add-keywords
     'scheme-mode
     ;; Types for Typed Racket.
     `((,(rx bow upper (* (syntax word)) eow)
        (0 font-lock-type-face))))))

(cb:declare-package-installer python
  :match "\\.py"
  :packages (python
             python-info
             virtualenvwrapper))

(hook-fn 'python-mode-hook
  (run-hooks 'prog-mode-hook))

(after 'python
  (require 'pyvenv)
  (require 'virtualenvwrapper)

(define-key python-mode-map (kbd ",") 'cb:comma-then-space)
(define-key inferior-python-mode-map (kbd ",") 'cb:comma-then-space)

(after 'evil
  (define-evil-doc-handler cb:python-modes
    (call-interactively 'rope-show-doc)))

(defun cb-py:restart-python ()
  (save-window-excursion
    (let (kill-buffer-query-functions
          (buf (get-buffer "*Python*")))
      (when buf (kill-buffer buf)))
    (call-interactively 'run-python)))

(defun cb:switch-to-python ()
  "Switch to the last active Python buffer."
  (interactive)
  ;; Start inferior python if necessary.
  (unless (->> (--first-buffer (derived-mode-p 'inferior-python-mode))
            (get-buffer-process)
            (processp))
    (cb-py:restart-python))

  (if (derived-mode-p 'inferior-python-mode)
      ;; Switch from inferior python to source file.
      (switch-to-buffer-other-window
       (--first-buffer (derived-mode-p 'python-mode)))
    ;; Switch from source file to REPL.
    ;; HACK: `switch-to-buffer-other-window' does not change window
    ;; when switching to REPL buffer. Work around this.
    (-when-let* ((buf (--first-buffer (derived-mode-p 'inferior-python-mode)))
                 (win (or (--first-window (equal (get-buffer "*Python*")
                                                 (window-buffer it)))
                          (split-window-sensibly)
                          (next-window))))
      (set-window-buffer win buf)
      (select-window win)
      (goto-char (point-max))
      (cb:maybe-evil-append-line 1))))

(define-key python-mode-map (kbd "C-c C-z") 'cb:switch-to-python)
(define-key inferior-python-mode-map (kbd "C-c C-z") 'cb:switch-to-python)

(defun cb-py:eval-dwim (&optional arg)
  (interactive "P")
  (cond
   ((region-active-p)
    (python-shell-send-region (region-beginning) (region-end))
    (deactivate-mark))
   (t
    (python-shell-send-defun arg))))

(define-key python-mode-map (kbd "C-c C-c") 'cb-py:eval-dwim)

(defun cb-py:smart-equals ()
  "Insert an '=' char padded by spaces, except in function arglists."
  (interactive)
  (if (s-matches? (rx (* space) "def" space) (current-line))
      (insert "=")
    (smart-insert-op "=")))

(defun cb-py:smart-asterisk ()
  "Insert an asterisk with padding unless we're in an arglist."
  (interactive "*")
  (cond
   ((s-matches? (rx (* space) "def" space) (current-line))
    (insert "*"))
   ;; Collapse whitespace around exponentiation operator.
   ((thing-at-point-looking-at (rx (* space) "*" (* space)))
    (delete-horizontal-space)
    (save-excursion
      (search-backward "*")
      (delete-horizontal-space))
    (insert "*"))
   (t
    (smart-insert-op "*"))))

(defun cb-py:smart-comma ()
  "Insert a comma with padding."
  (interactive "*")
  (insert ",")
  (just-one-space))

(defun cb-py:smart-colon ()
  "Insert a colon with padding."
  (interactive "*")
  (insert ":")
  (just-one-space))

(--each '(python-mode inferior-python-mode)
  (declare-smart-ops it
    :add '("?" "$")
    :custom
    '(("," . cb-py:smart-comma)
      ("*" . cb-py:smart-asterisk)
      (":" . cb-py:smart-colon)
      ("=" . cb-py:smart-equals))))

(sp-with-modes cb:python-modes
  (sp-local-pair "{" "}" :post-handlers '(:add sp-generic-leading-space)))

(after 'autoinsert
  (define-auto-insert
    '("\\.py$" . "Python skeleton")
    '("Short description: "
      "\"\"\"\n"
      str
      "\n\"\"\"\n\n"
      _
      "\n")))

(defun cb-py:split-arglist (arglist)
  "Parse ARGLIST into a list of parameters.
Each element is either a string or a cons of (var . default)."
  (cl-loop
   for arg in (s-split (rx ",") arglist t)
   for (x . y)  = (s-split "=" arg)
   for (_ name) = (s-match (rx (* (any "*")) (group (* (any "_" alnum)))) x)
   for default  = (when y (car y))
   when (not (s-blank? (s-trim name)))
   collect (if default (cons name default) name)))

(defun cb-py:python-docstring (arglist)
  "Format a docstring according to ARGLIST."
  (let ((al (s-replace " " "" arglist)))
    (if (s-blank? al)
        ""
      (cl-destructuring-bind (keywords formal)
          (-separate 'listp (cb-py:split-arglist al))
        (concat
         (when (or formal keywords) "\n")
         ;; Formal args
         (when (and formal keywords) "    Formal arguments:\n")
         (s-join "\n" (--map (format "    %s --" it) formal))
         (when keywords "\n\n")
         ;; Keyword args
         (when (and formal keywords) "    Keyword arguments:\n")
         (s-join "\n" (--map (format "    %s (default %s) --" (car it) (cdr it))
                             keywords)))))))

(defun cb-py:arglist-for-function-at-point ()
  "Return the arglist for the function at point, or nil if none."
  (save-excursion
    (when (beginning-of-defun)
      (let ((start (search-forward "("))
            (end (1- (search-forward ")"))))
        (buffer-substring start end)))))

(defun cb-py:insert-docstring ()
  "Insert a docstring for the python function at point."
  (interactive "*")
  (-when-let (arglist (cb-py:arglist-for-function-at-point))
    (when (beginning-of-defun)
      (search-forward-regexp (rx ":" (* space) eol))
      (newline)
      (open-line 1)
      (insert (concat "    \"\"\"\n"
                      (cb-py:python-docstring arglist) "\n\n"
                      "    Returns:\n\n"
                      "    \"\"\"" ))
      (message "Arglist inserted."))))

(add-to-list 'insertion-picker-options
             '("d" "Docstring" cb-py:insert-docstring :modes (python-mode)))

(define-key python-mode-map (kbd "M-q") 'indent-dwim)

(venv-initialize-eshell)

(when (fboundp 'ropemacs-mode)
  (add-hook 'python-mode-hook 'ropemacs-mode))

(setq ropemacs-use-pop-to-buffer t
      ropemacs-guess-project t)

(after 'rope
  (define-key python-mode-map (kbd "M-.") 'rope-goto-definition))

(after 'evil
  (evil-define-key 'normal python-mode-map (kbd "M-.") 'rope-goto-definition))

)

(cb:declare-package-installer ruby
  :match (rx (or "Rakefile" "Vagrantfile" "Thorfile" "Capfile" "GuardFile" "Gemfile"
                 ".rb" ".ru" ".rake" ".jbuilder" ".thor" ".gemspec" ".podspec"))
  :packages (ruby-mode
             inf-ruby
             rvm
             rubocop))

(cb:declare-package-installer yaml
  :match (rx ".ya" (? "ml") eol)
  :packages (yaml-mode))

(add-to-list 'completion-ignored-extensions ".rbc")

(-each '(("\\.rake\\'". ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("Capfile\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode)
         ("Thorfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode))
       (~ add-to-list 'auto-mode-alist))

(define-derived-mode erb-mode html-mode
  "ERB" nil
  (when (fboundp 'flycheck-mode)
    (flycheck-mode -1)))

(add-to-list 'auto-mode-alist '("\\.html\\.erb" . erb-mode))

(after 'ruby-mode

(let ((file (f-join (-first (~ s-matches? "inf-ruby") (f-directories cb:elpa-dir))
                    "inf-ruby.el")))

  (autoload 'inf-ruby-mode file nil t)
  (load-file file))

(after '(ruby-mode inf-ruby)

(defadvice ruby-switch-to-inf (around start-inf-ruby activate)
  "Start an inferior ruby if one is not running."
  (condition-case _
      ad-do-it
    (wrong-type-argument
     (run-ruby))))

(defun set-ruby-interpreter (cmd)
  "Set the default ruby interpreter to CMD."
  (interactive
   (list
    (ido-completing-read
     "Inferior Ruby Program: "
     (->> inf-ruby-implementations
       (-map 'car)
       (-filter 'executable-find)))))
  (setq inf-ruby-default-implementation cmd))

(defun cb-rb:inf-ruby-window ()
  (-when-let (buf (get-buffer inf-ruby-buffer))
    (--first-window (equal (window-buffer it) buf))))

(defun restart-ruby ()
  (interactive)
  ;; Suppress exit query.
  (-when-let (proc (ignore-errors (inf-ruby-proc)))
    (set-process-query-on-exit-flag proc nil))
  ;; Kill and relaunch IRB, reusing existing window.
  (let ((win (cb-rb:inf-ruby-window)))
    (ignore-errors (kill-buffer inf-ruby-buffer))
    (save-window-excursion (run-ruby))
    (when win
      (set-window-buffer win inf-ruby-buffer))))

(defun cb-rb:switch-to-ruby ()
  "Toggle between irb and the last ruby buffer.
Start an inferior ruby if necessary."
  (interactive)
  (cond
   ((derived-mode-p 'inf-ruby-mode)
    (switch-to-buffer-other-window
     (--first-buffer (derived-mode-p 'ruby-mode))))
   ((and inf-ruby-buffer (get-buffer inf-ruby-buffer))
    (ruby-switch-to-inf t))
   (t
    (run-ruby))))

(define-key ruby-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)
(define-key inf-ruby-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)
(define-key inf-ruby-minor-mode-map (kbd "C-c C-z") 'cb-rb:switch-to-ruby)

(defun cb-rb:eval-dwim ()
  "Perform a context-sensitive evaluation."
  (interactive)
  ;; Start ruby if necessary.
  (unless (get-buffer "*ruby*")
    (run-ruby)
    (cb-rb:switch-to-ruby)
    ;; Revert window layout.
    (when (= 2 (length (window-list)))
      (delete-other-windows)))
  (cond
   ;; Evaluate active region.
   ((region-active-p)
    (ruby-send-region (region-beginning) (region-end)))
   ;; Evaluate the block at or just before point.
   ((or (thing-at-point-looking-at
         (rx (or "end" "]" "}" ")") (* space) (* "\n")))
        (ruby-block-contains-point (point)))
    (ruby-send-block))
   ;; Eval the block-like thing around point.
   (t
    (ruby-send-region (line-beginning-position)
                      (line-end-position)))))

(define-key ruby-mode-map (kbd "C-c C-c") 'cb-rb:eval-dwim)

(defun cb-rb:format-irb-error (lines)
  "Return a propertized error string for the given LINES of
an irb error message."
  (-when-let* ((err (--first (s-matches? "Error:" it) lines))
               (colon (s-index-of ":" err)))
    (concat (propertize (substring err 0 colon) 'face 'error)
            (substring err colon))))

(defun cb-rb:apply-font-lock (str)
  "Apply ruby font-locking to string STR."
  (with-temp-buffer
    (insert str)
    (require 'ruby-mode)
    ;; Configure ruby font-lock.
    (set (make-local-variable 'font-lock-defaults)
         '((ruby-font-lock-keywords) nil nil))
    (set (make-local-variable 'syntax-propertize-function)
         'ruby-syntax-propertize-function)

    (font-lock-fontify-buffer)
    (buffer-string)))

(defun cb-rb:filter-irb-output (str &rest _)
  "Print IRB output to messages."
  (ignore-errors
    (when (and (fboundp 'inf-ruby-proc) (inf-ruby-proc))
      (let ((lines
             (->> (s-lines str)
               (--remove (or (s-contains? "--inf-ruby" it)
                             (s-blank? it)
                             (s-matches? inf-ruby-prompt-pattern it)))
               (-map 's-trim))))
        (message (or (cb-rb:format-irb-error lines)
                     (cb-rb:apply-font-lock (car (reverse lines))))))))
  str)

(hook-fn 'inf-ruby-mode-hook
  (add-hook 'comint-preoutput-filter-functions 'cb-rb:filter-irb-output)
  ;; Stop IRB from echoing input.
  (setq comint-process-echoes t))

)

(defun cb-rb:rockets->colons ()
  "Convert old-style rockets to new hash literal syntax in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (rx ":" (group-n 1 (+ (not space)))
                                      (* space)
                                      "=>"
                                      (* space))
                                  nil t)
      (replace-match "\\1: " t nil))))

(add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)

(add-hook 'cb:ruby-modes-hook 'subword-mode)

(defun cb-rb:smart-colon ()
  "Insert a colon, with or without padding.
If this is the leading colon for a symbol, do not insert padding.
If this is the trailing colon for a hash key, insert padding."
  (interactive)
  (insert ":")
  (when (s-matches? (rx (+ alnum) ":" eol)
                    (buffer-substring (line-beginning-position) (point)))
    (just-one-space)))

(--each cb:ruby-modes
  (declare-smart-ops it
    :add '("~")
    :custom
    '(("," . (command (insert ",") (just-one-space)))
      (":" . cb-rb:smart-colon))))

(require 'smartparens-ruby)

(modify-syntax-entry ?@ "w" ruby-mode-syntax-table)
(modify-syntax-entry ?_ "w" ruby-mode-syntax-table)
(modify-syntax-entry ?! "w" ruby-mode-syntax-table)
(modify-syntax-entry ?? "w" ruby-mode-syntax-table)

(defun sp-ruby-should-insert-pipe-close (_id _action _ctx)
  "Test whether to insert the closing pipe for a lambda-binding pipe pair."
  (thing-at-point-looking-at
   (rx-to-string `(and (or "do" "{") (* space) "|"))))

(defun sp-ruby-sp-hook-space-before (_id action _ctx)
  "Move to point before ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-backward "|")
      (just-one-space))))

(defun sp-ruby-sp-hook-space-after (_id action _ctx)
  "Move to point after ID and insert a space."
  (when (equal 'insert action)
    (save-excursion
      (search-forward "|")
      (just-one-space))))

(sp-with-modes '(ruby-mode inf-ruby-mode)

  (sp-local-pair "{" "}"
                 :post-handlers '(:add sp-generic-leading-space))

  (sp-local-pair "[" "]"
                 :pre-handlers '(sp-ruby-pre-handler))

  (sp-local-pair "%q{" "}" :when '(sp-in-code-p))
  (sp-local-pair "%Q{" "}" :when '(sp-in-code-p))
  (sp-local-pair "%w{" "}" :when '(sp-in-code-p))
  (sp-local-pair "%W{" "}" :when '(sp-in-code-p))
  (sp-local-pair  "%(" ")" :when '(sp-in-code-p))
  (sp-local-pair "%x(" ")" :when '(sp-in-code-p))
  (sp-local-pair  "#{" "}" :when '(sp-in-string-p))

  (sp-local-pair "|" "|"
                 :when '(sp-ruby-should-insert-pipe-close)
                 :unless '(sp-in-string-p)
                 :pre-handlers '(sp-ruby-sp-hook-space-before)
                 :post-handlers '(sp-ruby-sp-hook-space-after))

  (sp-local-pair "case" "end"
                 :when '(("SPC" "RET" "<evil-ret>"))
                 :unless '(sp-ruby-in-string-or-word-p)
                 :actions '(insert)
                 :pre-handlers '(sp-ruby-pre-handler)
                 :post-handlers '(sp-ruby-block-post-handler)))

(hook-fn 'cb:ruby-modes-hook
  (local-set-key (kbd "C-c C-h") 'yari))

(after 'hideshow
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                 ,(rx (or "}" "]" "end"))                  ; Block end
                 ,(rx (or "#" "=begin"))                   ; Comment start
                 ruby-forward-sexp nil)))

(after 'evil
  (define-evil-doc-handler cb:ruby-modes (call-interactively 'robe-doc)))

)

(cb:declare-package-installer scala
  :match (rx (or ".scala" ".sbt"))
  :packages (scala-mode2))

(after 'scala-mode2

(setq scala-indent:align-forms t
      scala-indent:align-parameters t
      scala-indent:default-run-on-strategy scala-indent:eager-strategy)

(defun cbscala:find-case-class-parent ()
  (save-excursion
    (if (search-backward-regexp
         (rx (or
              (and bol (* space)
                   (or (and (? "abstract" (+ space)) "class")
                       "trait")
                   (+ space) (group-n 1 (+ alnum)))
              (and bol (* space)
                   "case" (+ space) "class" (* anything) space
                   "extends" (+ space) (group-n 1 (+ alnum)) (* space) eol)))
         nil t)
        (match-string 1)
      "")))

(defun cbscala:equals ()
  (interactive)
  (smart-insert-op "=")
  (just-one-space))

(defun cbscala:colon ()
  (interactive)
  (smart-insert-op ":")
  (just-one-space))

(defmacro define-scala-variance-op-command (sym op)
  "Define command named SYM to insert a variance operator OP."
  `(defun ,sym ()
     "Insert a variance operator.
Pad in normal expressions. Do not insert padding in variance annotations."
     (interactive "*")
     (cond
      ;; No padding at the start of type parameter.
      ((thing-at-point-looking-at (rx "[" (* space)))
       (delete-horizontal-space)
       (insert ,op))
      ;; Leading padding after a comma, e.g. for a type parameter or function call.
      ((thing-at-point-looking-at (rx "," (* space)))
       (just-one-space)
       (insert ,op))
      ;; Otherwise leading and trailing padding.
      (t
       (smart-insert-op ,op)))))

(define-scala-variance-op-command cbscala:plus "+")
(define-scala-variance-op-command cbscala:minus "-")

(declare-smart-ops 'scala-mode
  :custom
  '(("=" . cbscala:equals)
    (":" . cbscala:colon)
    ("+" . cbscala:plus)
    ("-" . cbscala:minus)))

(define-key scala-mode-map (kbd ".") nil)

(after 'evil

  (defun cbscala:join-line ()
    "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
    (interactive)
    (let (join-pos)
      (save-excursion
        (goto-char (line-end-position))
        (unless (eobp)
          (forward-line)
          (call-interactively 'scala-indent:join-line)
          (setq join-pos (point))))

      (when join-pos
        (goto-char join-pos))))

  (evil-define-key 'normal scala-mode-map "J" 'cbscala:join-line))

)

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

;; (after 'haskell-interactive-mode
;;   (require 'haskell-mode))

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

(cb:declare-package-installer idris
  :match (rx ".idr" eol)
  :packages (idris-mode))

(add-to-list 'completion-ignored-extensions ".ibc")

(setq idris-warnings-printing 'warnings-repl)

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
    (smart-insert-op ":"))
   ((and (char-after) (equal (char-to-string (char-after)) ")"))
    (insert ":"))
   (t
    (smart-insert-op ":"))))

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
      (smart-insert-op "|")))))


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
      (smart-insert-op ".")))))

(defun cbidris:smart-question-mark ()
  "Insert a ? char as an operator, unless point is after an = sign."
  (interactive)
  (cond
   ((s-matches? (rx "=" (* space) eol) (current-line))
    (just-one-space)
    (insert "?"))
   (t
    (smart-insert-op "?"))))

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

(cb:declare-package-installer standard-ml
  :match (rx "." (or "sml" "sig" "cm" "grm"))
  :packages (sml-mode))

(setq sml-indent-level 2)
(add-to-list 'completion-ignored-extensions "\\.cm")
(add-to-list 'completion-ignored-extensions "\\.CM")

(cb:declare-package-installer ocaml
  :match (rx ".ml" (? (any "y" "i" "l" "p")))
  :packages (tuareg
             merlin))

(when (executable-find "opam")
  (cl-loop with env = (read-from-string (%-string "opam config env --sexp"))
           for (var val) in (car env)
           do (setenv var val)))

(when (fboundp 'cb:set-path-from-shell)
  (cb:set-path-from-shell))

(push (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../share/emacs/site-lisp")
      load-path)
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)

(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-default-flags "-w @A-4-33-41-42-43-34-44")
(add-to-list 'face-remapping-alist '(merlin-type-face . intense-flash))

(after '(tuareg merlin)
  (define-key merlin-mode-map (kbd "M-N") 'merlin-error-next))

(defun cb-ocaml:at-prompt-bol? ()
  (s-matches? (rx bol "utop[" (+ digit) "]>" (* space) (* word) eol)
              (buffer-substring (line-beginning-position) (point))))

(after 'tuareg
  (require 'ocp-indent (f-join cb:lib-dir "ocp-indent.el"))

  (defun ocp-indent-dwim ()
    "Perform a context-sensitive indentation command."
    (interactive)
    (cond
     ((region-active-p)
      (call-interactively 'ocp-indent-region)
      (message "ocp: Indented region"))
     ((thing-at-point 'defun)
      (ocp-indent-region (save-excursion (beginning-of-defun) (point))
                         (save-excursion (end-of-defun) (point)))
      (message "ocp: Indented defun"))
     (t
      (ocp-indent-region (point-min) (point-max))
      (message "ocp: Indented buffer"))))

  (define-key tuareg-mode-map (kbd "M-q") 'ocp-indent-dwim))

(sp-with-modes '(tuareg-mode utop-mode)
  (sp-local-pair "[|" "|]")
  (sp-local-pair "{<" ">}")
  (sp-local-pair "`" nil :actions nil))

(defun cb-ocaml:just-inserted-double-quotes? (id action ctx)
  (and (sp-in-string-p id action ctx)
       (s-matches? (rx (not (any "\\")) "\"" eol)
                   (buffer-substring (line-beginning-position) (point)))))

(defun sp-ocaml-just-one-space (id action ctx)
  "Pad delimiters with spaces."
  (when (and (equal 'insert action)
             (or (sp-in-code-p id action ctx)
                 (cb-ocaml:just-inserted-double-quotes? id action ctx)))
    ;; Insert a leading space, unless
    ;; 1. this is the first position of another list
    ;; 2. this form begins a new line.
    ;; 3. this form is preceded by a `?`, as in a let binding.
    ;; 4. this form is preceded by a `:`, as in a keyword argument
    ;; 4. this form is preceded by a `.`, as in an array index expression
    (save-excursion
      (search-backward id)
      (unless (s-matches?
               (rx (or (group bol (* space))
                       (any "." "," ":" "(" "[" "[|" "{" "?")
                       ;; HACK: utop prompt
                       (and "utop[" (+ digit) "]" ">" (* space)))
                   eol)
               (buffer-substring (line-beginning-position) (point)))
        (just-one-space)))
    ;; Insert space after separator, unless
    ;; 1. this form is at the end of another list.
    ;; 2. this form is at the end of the line.
    (save-excursion
      (search-forward (sp-get-pair id :close))
      (unless (s-matches? (rx (or (any ")" "]" "|]" "}") eol))
                          (char-to-string (char-after)))
        (just-one-space)))))

(sp-with-modes '(tuareg-mode utop-mode)
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{" "}" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[" "]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "(" ")" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "'" nil :actions nil))

(defun cb-ocaml:maybe-pad-parens ()
  (save-excursion
    (unless (equal (string-to-char " ") (char-before))
      (forward-char -1)
      (when (-contains? '(?\) ?\]) (char-before))
        (just-one-space)))))

(hook-fns '(tuareg-mode-hook utop-mode-hook)
  (add-hook 'post-self-insert-hook 'cb-ocaml:maybe-pad-parens nil t))

(defconst cb-ocaml:smart-operator-list
  (list "!" "$" "%" "&" "*" "+" "-" "." "/"
        ":" "<" "=" ">" "?" "@" "^" "|" "~"))

(--each '(tuareg-mode utop-mode)
  (put it 'smart-op-list cb-ocaml:smart-operator-list))

(defun cb-ocaml:smart-dot ()
  "Smart period for OCaml."
  (interactive)
  (yas-with-field-restriction
    (delete-horizontal-space 'back)
    (insert ".")
    (when (thing-at-point-looking-at (rx (any "+" "/" "-" "*") "."))
      (just-one-space))))

(defun cb-ocaml:smart-colon ()
  "Insert a context-sensitive smart colon."
  (interactive)
  (cond
   ;; Application of named parameter.
   ((thing-at-point-looking-at (rx (or "?" "~") (+ word)))
    (insert ":"))
   ;; Named parameter in val binding.
   ((and (s-matches? (rx bol (* space) "val" symbol-end)
                     (current-line))
         (save-excursion
           (search-backward "->" (line-beginning-position) t)))
    (delete-horizontal-space 'back)
    (insert ":"))
   (t
    (smart-insert-op ":"))))

(defun cb-ocaml:smart-asterisk ()
  "Smart asterisk for OCaml."
  (interactive)
  (yas-with-field-restriction

    (cond
     ;; Additional star for docstring comments of the type:
     ;;
     ;;    (*| *) -> (** | *)
     ;;
     ((s-matches? (rx "(" (+ "*") (* space) eol) (buffer-substring
                                           (line-beginning-position)
                                           (point)))
      (delete-horizontal-space 'back)
      (insert "*")
      (just-one-space 2)
      (forward-char -1))
     (t
      (cb-ocaml:smart-insert-operator "*")))))

(defun cb-ocaml:smart-semicolon ()
  "Smart semicolon for OCaml."
  (interactive)
  (yas-with-field-restriction
    (delete-horizontal-space 'back)
    (insert ";")
    (unless (thing-at-point-looking-at ";;")
      (just-one-space))))

(defun cb-ocaml:smart-insert-operator (op)
  "Perform a smart insertion of operator OP, unless inside parens."
  (if (thing-at-point-looking-at (rx "("))
      (insert op)
    (smart-insert-op op)))

(defun cb-ocaml:smart-pipe ()
  "Insert either the pipe chars in an array literal or a smart pipe."
  (interactive)
  (cond ((thing-at-point-looking-at (rx "[]"))
         (insert "|")
         (save-excursion
           (insert "|")))
        (t
         (cb-ocaml:smart-insert-operator "|"))))

(defmacro cb-ocaml:define-smart-op-as-annotation (symbol op)
  "Define a command for inserting a smart operator.
This is for operators that also have special meanings in binding forms
where they should not be padded."
  `(defun ,symbol ()
     "Auto-generated smart operator command for OCaml."
     (interactive)
     (cb-ocaml:smart-insert-operator ,op)
     (when (thing-at-point-looking-at (rx space))
       (delete-horizontal-space 'back))))

(cb-ocaml:define-smart-op-as-annotation cb-ocaml:smart-tilde "~")
(cb-ocaml:define-smart-op-as-annotation cb-ocaml:smart-question "?")

(defun cb-ocaml:set-keys (keymap)
  "Set smart operators for OCaml."
  (--each cb-ocaml:smart-operator-list
    (define-key keymap (kbd it)
      (eval `(command (cb-ocaml:smart-insert-operator ,it)))))

  (define-key keymap (kbd "!") nil)
  (define-key keymap (kbd "*") 'cb-ocaml:smart-asterisk)
  (define-key keymap (kbd ".") 'cb-ocaml:smart-dot)
  (define-key keymap (kbd "|") 'cb-ocaml:smart-pipe)
  (define-key keymap (kbd ":") 'cb-ocaml:smart-colon)
  (define-key keymap (kbd ";") 'cb-ocaml:smart-semicolon)
  (define-key keymap (kbd "?") 'cb-ocaml:smart-question)
  (define-key keymap (kbd "~") 'cb-ocaml:smart-tilde))

(after 'tuareg (cb-ocaml:set-keys tuareg-mode-map))
(after 'utop   (cb-ocaml:set-keys utop-mode-map))

(defun cb-ocaml:newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (insert str))

(defun cb-ocaml:newline-and-expand-snippet-at-col (predicate col)
  "Insert a new line, find the template matching PREDICATE and insert at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (yas-insert-first-snippet predicate))

(defun cb-ocaml:case-start-col ()
  (save-excursion
    (goto-char (line-beginning-position))
    (search-forward "|")
    (1- (current-column))))

(defun cb-ocaml:meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond
   ;; Insert case after function keyword.
   ((s-matches? (rx symbol-start "function" (* space) eol) (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert case after match statement.
   ((s-matches? (rx symbol-start "match" symbol-end) (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) "type") (current-line))
    (cb-ocaml:newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (+ 2 (current-column)))
     "| "))

   ;; Insert line starting with pipeline below the current one.
   ((s-matches? (rx bol (* space) "|>") (current-line))
    (cb-ocaml:newline-and-insert-at-col (cb-ocaml:case-start-col) "|> "))

   ;; Insert match case below the current one.
   ((s-matches? (rx bol (* space) "|" (* nonl) "->") (current-line))
    (cb-ocaml:newline-and-expand-snippet-at-col
     (C (~ equal "match-case") yas--template-name)
     (cb-ocaml:case-start-col)))

   ;; Insert any other kind of case below the current one.
   ((s-matches? (rx bol (* space) "|") (current-line))
    (cb-ocaml:newline-and-insert-at-col (cb-ocaml:case-start-col) "| "))

   ;; Insert new val binding below the current one.
   ((s-matches? (rx bol (* space) "val" symbol-end) (current-line))
    (let ((col (save-excursion
                 (goto-char (line-beginning-position))
                 (forward-to-indentation 0)
                 (current-column))))

      (goto-char (line-end-position))
      (newline)
      (indent-to col)
      (yas-insert-first-snippet (C (~ equal "val") yas--template-name))))

   ;; Insert new let...in binding below the current one.
   ((s-matches? (rx (group "try") symbol-end (* nonl) "with" (* space) eol)
                (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new let...in binding below the current one.
   ((s-matches? (rx bol (* space) "with" (* space) "\n"
                    (* nonl) "->")
                (buffer-substring (save-excursion (forward-line -1)
                                                  (line-beginning-position))
                                  (line-end-position)))
    ;; Insert pipe for case if needed.
    (back-to-indentation)
    (unless (thing-at-point-looking-at "|")
      (insert "| "))
    ;; Insert case.
    (goto-char (line-end-position))
    (newline-and-indent)
    (forward-char -2)
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new let...in binding below the current one.
   ((s-matches? (rx bol (* space) (group "let") symbol-end (* nonl) "in" (* space) eol)
                (current-line))
    (cb-ocaml:newline-and-expand-snippet-at-col
     (C (~ equal "let...in") yas--template-name)
     (save-excursion
       (or (search-backward-regexp
            (rx symbol-start "let" symbol-end)
            (line-beginning-position) t)
           (goto-char (line-beginning-position)))
       (current-column))))

   ;; Insert new top-level let binding below the current one.
   ((s-matches? (rx bol (* space) (group "let") symbol-end) (current-line))
    (cb-ocaml:newline-and-expand-snippet-at-col
     (C (~ equal "let (top-level)") yas--template-name)
     (save-excursion
       (or (search-backward-regexp
            (rx symbol-start "let" symbol-end)
            (line-beginning-position) t)
           (goto-char (line-beginning-position)))
       (current-column))))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (when (true? evil-mode)
    (evil-insert-state)))

(after 'tuareg (define-key tuareg-mode-map (kbd "M-RET") 'cb-ocaml:meta-ret))
(after 'utop   (define-key utop-mode-map   (kbd "M-RET") 'cb-ocaml:meta-ret))

(after 'tuareg
  (define-key tuareg-mode-map (kbd "C-c C-f") 'tuareg-eval-buffer)
  (define-key tuareg-mode-map (kbd "C-c C-r") 'tuareg-eval-region)
  (define-key tuareg-mode-map (kbd "C-c C-c") 'tuareg-eval-phrase))

(after 'tuareg

  (defun cb-ocaml:switch-to-utop ()
    (interactive)
    (unless (get-buffer "*utop*")
      (save-window-excursion (utop)))
    (pop-to-buffer "*utop*")
    (goto-char (point-max))
    (when (true? evil-mode)
      (evil-insert-state)))

  (define-key tuareg-mode-map (kbd "C-c C-z") 'cb-ocaml:switch-to-utop))

(after 'utop

  (defun cb-ocaml:switch-to-src ()
    (interactive)
    (-if-let (buf (--first-buffer (derived-mode-p 'tuareg-mode)))
        (pop-to-buffer buf)
      (message "No active OCaml buffers")))

  (define-key utop-mode-map (kbd "C-c C-z") 'cb-ocaml:switch-to-src))

(require 'proof-site (f-join cb:lib-dir "proofgeneral" "generic" "proof-site"))

(after 'proof-script
  (define-derived-mode proof-mode prog-mode
    proof-general-name
    "Proof General major mode class for proof scripts.
\\{proof-mode-map}"

    (setq proof-buffer-type 'script)

    ;; Set default indent function (can be overriden in derived modes)
    (make-local-variable 'indent-line-function)
    (setq indent-line-function 'proof-indent-line)

    ;; During write-file it can happen that we re-set the mode for the
    ;; currently active scripting buffer.  The user might also do this
    ;; for some reason.  We could maybe let this pass through, but it
    ;; seems safest to treat it as a kill buffer operation (retract and
    ;; clear spans).  NB: other situations cause double calls to proof-mode.
    (if (eq (current-buffer) proof-script-buffer)
        (proof-script-kill-buffer-fn))

    ;; We set hook functions here rather than in proof-config-done so
    ;; that they can be adjusted by prover specific code if need be.
    (proof-script-set-buffer-hooks)

    ;; Set after change functions
    (proof-script-set-after-change-functions)

    (add-hook 'after-set-visited-file-name-hooks
              'proof-script-set-visited-file-name nil t)

    (add-hook 'proof-activate-scripting-hook 'proof-cd-sync nil t)))

(setq proof-splash-enable nil)

(defvar cb-coq:smart-operator-list
  '("!" "$" "%" "&" "*" "+" "-" "." "/" ":" "<" "=" ">" "?" "@" "^" "|" "~")
  "The list of operators in the Coq language.")

(put 'coq-mode 'smart-operator-alist cb-coq:smart-operator-list)

(defun cb-coq:smart-insert-operator (op)
  "Perform a smart insertion of operator OP, unless inside parens."
  (if (thing-at-point-looking-at (rx "("))
      (insert op)
    (smart-insert-op op)))

(defun cb-coq:smart-pipe ()
  "Insert either the pipe chars in an array literal or a smart pipe."
  (interactive)
  (cond ((thing-at-point-looking-at (rx "[]"))
         (insert "|")
         (save-excursion
           (insert "|")))
        (t
         (cb-coq:smart-insert-operator "|"))))

(defun cb-coq:set-keys ()
  "Set smart operators for Coq."
  (--each cb-coq:smart-operator-list
    (define-key coq-mode-map (kbd it)
      (eval `(command (cb-ocaml:smart-insert-operator ,it)))))

  (define-key coq-mode-map (kbd "!") nil)
  (define-key coq-mode-map (kbd ".") nil)
  (define-key coq-mode-map (kbd "|") 'cb-ocaml:smart-pipe))

(after 'coq (cb-coq:set-keys))

(defun cb-coq:case-start-col ()
  (save-excursion
    (goto-char (line-beginning-position))
    (search-forward "|")
    (1- (current-column))))

(defun cb-coq:newline-and-insert-at-col (col str)
  "Insert STR on a new line at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (insert str))

(defun cb-coq:newline-and-expand-snippet-at-col (predicate col)
  "Insert a new line, find the template matching PREDICATE and insert at COL."
  (goto-char (line-end-position))
  (newline)
  (indent-to col)
  (yas-insert-first-snippet predicate))

(defun cb-coq:rx-start-column (rx)
  (save-excursion
    (goto-char (line-end-position))
    (search-backward-regexp rx (line-beginning-position))
    (current-column)))

(defun cb-coq:meta-ret ()
  "Open a new line in a context-sensitive way."
  (interactive)
  (yas-exit-all-snippets)
  (cond

   ;; Insert case after match statement.
   ((s-matches? (rx symbol-start "match" symbol-end) (current-line))
    (goto-char (line-end-position))
    (newline-and-indent)
    (goto-char (max (line-beginning-position) (- (point) 2)))
    (yas-insert-first-snippet (C (~ equal "match-case") yas--template-name)))

   ;; Insert new case below the current type decl.
   ((s-matches? (rx bol (* space) "Inductive") (current-line))
    (cb-coq:newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert dependent type below the current one.
   ((s-matches? (rx bol (* space) "|" (* space) (+ word) (+ space) ":" (+ space))
                (current-line))
    (cb-coq:newline-and-insert-at-col
     (save-excursion
       (goto-char (line-beginning-position))
       (forward-to-indentation 0)
       (current-column))
     "| "))

   ;; Insert match case below the current one.
   ((s-matches? (rx bol (* space) "|" (* nonl) "=>") (current-line))
    (cb-coq:newline-and-expand-snippet-at-col
     (C (~ equal "match-case") yas--template-name)
     (cb-coq:case-start-col)))

   ;; Insert any other kind of case below the current one.
   ((s-matches? (rx bol (* space) "|") (current-line))
    (cb-coq:newline-and-insert-at-col (cb-coq:case-start-col) "| "))

   ;; Insert check.
   ((s-matches? (rx bol "Check") (current-line))
    (cb-coq:newline-and-expand-snippet-at-col
     (C (~ equal "Check") yas--template-name)
     (cb-coq:rx-start-column "Check")))

   (t
    (goto-char (line-end-position))
    (newline-and-indent)))

  (when (true? evil-mode)
    (evil-insert-state)))

(after 'coq (define-key coq-mode-map   (kbd "M-RET") 'cb-coq:meta-ret))

(sp-with-modes '(coq-mode)
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{" "}" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[" "]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "(" ")" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "'" nil :actions nil))

(after 'proof-script
  (define-key proof-mode-map (kbd "C-<return>") nil))

(after 'proof-faces
  (add-to-list 'face-remapping-alist '(proof-locked-face . hl-line))
  (add-to-list 'face-remapping-alist '(proof-error-face . error)))

(cb:declare-package-installer fsharp
  :match (rx ".fs" (? (any "i" "y" "l" "x")) eol)
  :packages (fsharp-mode))

(sp-with-modes '(fsharp-mode)
  (sp-local-pair "[|" "|]")
  (sp-local-pair "[<" ">]")
  (sp-local-pair "`" nil :actions nil))

(sp-with-modes '(fsharp-mode)
  (sp-local-pair "\"" "\"" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "{" "}" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "[" "]" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "(" ")" :post-handlers '(:add sp-ocaml-just-one-space))
  (sp-local-pair "'" nil :actions nil))

(hook-fns '(fsharp-mode)
  (add-hook 'post-self-insert-hook 'cb-ocaml:maybe-pad-parens nil t))

(put 'fsharp-mode 'smart-operator-alist cb-ocaml:smart-operator-list)
(after 'fsharp-mode (cb-ocaml:set-keys fsharp-mode-map))

(after 'fsharp-mode
  (define-key fsharp-mode-map (kbd "M-RET") 'cb-ocaml:meta-ret))

(hook-fn 'sh-mode-hook
  (when (executable-find "shellcheck")
    (flycheck-select-checker 'sh-shellcheck)))

(after 'make-mode

(define-key makefile-mode-map (kbd "C-c C-c") nil)

(defun convert-leading-spaces-to-tabs ()
  "Convert sequences of spaces at the beginning of a line to tabs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (rx bol (group (>= 4 space))) nil t)
      (replace-match "\t"))))

(hook-fn 'makefile-mode-hook
  (setq indent-tabs-mode t)
  (add-hook 'before-save-hook 'convert-leading-spaces-to-tabs nil t))

)

(cb:declare-package-installer tex
  :match (rx "." (or "tex" "dtx" "ins" "ltx" "sty"
                     "cls" "clo" "bbl" "dry" "lco") eol)
  :packages (auctex latex-preview-pane))

(add-to-list 'auto-mode-alist '("\\.lco$" . latex-mode))

(after 'tex

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1)))

(add-to-list 'face-remapping-alist '(bad-face . flycheck-error))

(latex-preview-pane-enable)

(defadvice TeX-complete-symbol (after position-point activate)
  "Position point inside braces."
  (when (equal (char-before) ?\})
    (forward-char -1)))

(require 'whizzytex)

(defvar whizzytex-sty-installation "/usr/local/share/whizzytex/latex/whizzytex.sty"
  "Path to the whizzytex macro package.")

(defvar whizzytex-src (f-join cb:lib-dir "whizzytex" "src")
  "Path to the whizzytex sources.")

(defvar whizzy-command-name (f-join whizzytex-src "whizzytex"))

(defun cbwh:install-tex-macros ()
  "Prompt the user to install the tex macros if they do not exist."
  (unless (f-exists? whizzytex-sty-installation)
    (when (y-or-n-p (format "Install whizzytex macros into %s? "
                            (f-dirname whizzytex-sty-installation)))
      ;; Make installation directory and copy package there.
      (%-sudo (%-sh "mkdir -p" (f-dirname whizzytex-sty-installation)))
      (%-sudo (%-sh "cp -f"
                    (%-quote (f-join whizzytex-src "whizzytex.sty"))
                    (%-quote whizzytex-sty-installation))))))

(hook-fn 'tex-mode-hook
  (cbwh:install-tex-macros)
  (whizzytex-mode +1))

(TeX-global-PDF-mode +1)

(require 'preview)
(require 'latex)

(after 'flycheck
  (bind-keys
    :map TeX-mode-map
    "M-P" 'flycheck-previous-error
    "M-N" 'flycheck-next-error
    "TAB" 'TeX-complete-symbol))

(autoload 'TeX-fold-mode "tex-fold")
(hook-fns '(tex-mode-hook latex-mode-hook)
  (TeX-fold-mode +1))

(after '(evil tex)
  (evil-define-key 'normal TeX-mode-map
    (kbd "z m") 'TeX-fold-buffer
    (kbd "z r") 'TeX-fold-clearout-buffer
    (kbd "SPC") 'TeX-fold-dwim))

)

(autoload 'sclang-mode "sclang")
(autoload 'sclang-start "sclang")
(add-to-list 'auto-mode-alist '("\\.sc$" . sclang-mode))

(defun supercollider ()
  "Start SuperCollider and open the SC Workspace."
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*sclang workspace*"))
  (sclang-mode))

(setq sclang-auto-scroll-post-buffer   t
      sclang-eval-line-forward         nil
      sclang-show-workspace-on-startup nil)

(hook-fn 'sclang-mode-hook
  (cb:install-package 'sclang-extensions)
  (sclang-extensions-mode +1))

(after 'sclang
  (define-key sclang-mode-map (kbd ".") nil))

(declare-smart-ops 'sclang-mode :rem '("|"))

(cb:declare-package-installer vimrc
  :match (rx "vimrc" eol)
  :packages (vimrc-mode))

(add-to-list 'auto-mode-alist '("vimrc$" . vimrc-mode))

(provide 'config-languages)
