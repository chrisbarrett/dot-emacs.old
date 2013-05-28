;;; cb-clang.el --- Configuration for C-like languages

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130526.2355

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

;; Configuration for C-like languages

;;; Code:

(require 'use-package)
(require 'dash)
(autoload 'smartparens-mode-map "smartparens")
(autoload 'c-mode-map "cc-mode")

(after 'cc-mode
  (define-key c-mode-map (kbd "C-c C-c") 'mode-compile)
  (define-key c-mode-map (kbd "M-q") 'indent-dwim)
  (require 'smart-operator)
  (require 'flycheck)
  (require 'smartparens))

(defun looking-at-c-flow-control-header? ()
  (thing-at-point-looking-at
   (rx (* nonl) (32 ";") (* space)
       (or "if" "when" "while" "for")
       (* nonl)
       "("
       (* (not (any ")"))))))

(defun looking-at-c-struct-initializer? ()
  (save-excursion
    (backward-up-list)
    (thing-at-point-looking-at
     (rx "=" (* space)
         ;; Optional casts
         (? (group "(" (* nonl) ")"))
         (* space)))))

(after 'smart-operator

  (defun c-insert-smart-op (str)
    "Insert a smart operator with special formatting in certain expressions."
    (if (looking-at-c-flow-control-header?)
        (insert str)
      (smart-insert-operator str)))

  (defun c-insert-smart-equals ()
    "Insert an '=' with context-sensitive formatting."
    (interactive)
    (if (or (looking-at-c-flow-control-header?)
            (looking-at-c-struct-initializer?))
        (insert "=")
      (smart-insert-operator "=")))

  (defun c-insert-smart-minus ()
    "Insert a minus with padding unless a unary minus is more appropriate."
    (interactive)
    (atomic-change-group
      (if (thing-at-point-looking-at
           (rx (or "return" "(" "[" "(" ";" "=") (* space)))
          (insert "-")
        (unless (->> (buffer-substring (line-beginning-position) (point))
                  (s-trim-left)
                  (s-blank?))
          (just-one-space))
        (insert "-")
        (just-one-space))))

  (defun c-insert-smart-star ()
    "Insert a * with padding in multiplication contexts."
    (interactive)
    (if (thing-at-point-looking-at (rx (not digit) (* space)))
        (insert "*")
      (unless (->> (buffer-substring (line-beginning-position) (point))
                (s-trim-left)
                (s-blank?))
        (just-one-space))
      (insert "*")
      (just-one-space)))

  (defun c-insert-smart-gt ()
    "Insert a > symbol with formatting.
If the insertion creates an right arrow (->), remove surrounding whitespace."
    (interactive)
    (c-insert-smart-op ">")
    ;; Check whether we just inserted the tip of an arrow. If we did,
    ;; remove the spacing surrounding the arrow.
    (when (thing-at-point-looking-at (rx "-" (* space) ">" (* space)))
      (save-excursion
        (let ((arrow-start (save-excursion
                             (search-backward-regexp
                              (rx (not (any space "-" ">"))))
                             (point))))
          (while (search-backward-regexp (rx space) arrow-start t)
            (delete-horizontal-space))))))

  (hook-fn 'c-mode-hook
    (macrolet ((c-smart-op (char) `(command (cb:c-insert-smart-op ,char))))
      (local-set-key (kbd ",") (command (insert ",") (just-one-space)))
      (local-set-key (kbd "=") (c-smart-op "="))
      (local-set-key (kbd "+") (c-smart-op "+"))
      (local-set-key (kbd "|") (smart-op "|"))
      (local-set-key (kbd ">") 'c-insert-smart-gt)
      (local-set-key (kbd "-") 'c-insert-smart-minus)
      (local-set-key (kbd "*") 'c-insert-smart-star))))

(after 'flycheck

  (defvar clang-custom-include-paths '("/usr/include/python2.7"))

  (defun s-alnum-only (s)
    "Remove non-alphanumeric characters from S."
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (search-forward-regexp (rx (not alnum)) nil t)
        (replace-match ""))
      (buffer-string)))

  (defun clang-parse-line-for-err (line)
    (ignore-errors
      (destructuring-bind (_ file line col level message)
          (s-match
           (rx (group-n 1 (+ nonl)) ":"  ; file
               (group-n 2 (+ digit)) ":" ; line
               (group-n 3 (+ digit)) ":" ; col
               (* space)
               (group-n 4 (+ nonl)) ":" ; level
               (* space)
               (group-n 5 (+ nonl))     ; message
               ) line)
        (flycheck-error-new
         :line (string-to-number line)
         :column (string-to-number col)
         :level (if (s-contains? "error" level) 'error 'warning)
         :message message
         :filename file))))

  (defun clang-error-parser (str &rest _)
    (->> (s-lines str) (-map 'clang-parse-line-for-err) (-remove 'null)))

  (defun pkg-config-installed-packages ()
    "Get the names of all known packages from pkg-config."
    (->> (shell-command-to-string "pkg-config --list-all")
      (s-lines)
      (--remove (s-starts-with? "Variable" it))
      (--map (car (s-split (rx space) it)))))

  (defvar pkg-config-all-includes
    (->> (pkg-config-installed-packages)
      (s-join " ")
      (concat "pkg-config --cflags ")
      (shell-command-to-string)
      ;; Ensure all includes end with a `/`.
      (s-replace "-I:" "-I")
      (s-split (rx (or bol (+ space)) "-I"))
      (-filter 'file-exists-p)
      (-concat clang-custom-include-paths)
      (--map (format "-I%s/" it)))
    "A string of cflags for including everything known to pkg-config.")

  (flycheck-declare-checker clang
    "Compiles the current file with clang."
    :command
    '("clang" "-O0" "-Wall"
      (eval pkg-config-all-includes)
      "-fsyntax-only"
      "-fno-color-diagnostics" "-fno-caret-diagnostics"
      "-fno-diagnostics-show-option"
      source-inplace)
    :error-parser 'clang-error-parser
    :predicate '(derived-mode-p 'c-mode))

  (add-to-list 'flycheck-checkers 'clang))

(after 'smartparens

  (defun c-insert-smart-lbrace ()
    "Insert a left brace, possibly with formatting."
    (interactive)
    (unwind-protect
        (atomic-change-group
          (if (thing-at-point-looking-at (rx ")" (* space) eol))
              ;; Insert and put braces on new lines.
              (progn
                (newline)
                (insert "{")
                (newline)
                (save-excursion (insert "\n}"))
                (c-indent-defun)
                (c-indent-line))
            ;; Do a smartparenish insertion.
            (progn
              (insert "{")
              (sp--self-insert-command 1)
              (save-excursion (search-backward "{")
                              (delete-char -1)))))))

  (define-key smartparens-mode-map (kbd "{")
    (command (if (derived-mode-p 'c-mode)
                 (c-insert-smart-lbrace)
               ;; HACK: simulate propert sp-insertion. I have no idea how
               ;; to do this cleanly.
               (insert "{")
               (sp--self-insert-command 1)
               (save-excursion (search-backward "{")
                               (delete-char -1)))))

  (defadvice sp--self-insert-command
    (before flow-control-keyword-insert-space activate)
    "Insert a space after a flow control keyword in c modes."
    (when  (and (derived-mode-p 'c-mode 'cc-mode 'c++-mode)
                (thing-at-point-looking-at
                 (rx symbol-start (or "if" "when" "while" "for"))))
      (just-one-space))))

(use-package cedit
  :commands
  (cedit-forward-char
   cedit-backward-char
   cedit-end-of-statement
   cedit-beginning-of-statement
   cedit-down-block
   cedit-up-block-backward
   cedit-up-block-forward
   cedit-slurp
   cedit-wrap-brace
   cedit-barf
   cedit-splice-killing-backward
   cedit-raise
   cedit-or-paredit-slurp
   cedit-or-paredit-barf
   cedit-or-paredit-splice-killing-backward
   cedit-or-paredit-raise))

(use-package google-c-style
  :ensure   t
  :defer    t
  :commands google-set-c-style
  :init    (add-hook 'c-mode-common-hook 'google-set-c-style))

(use-package disaster
  :ensure   t
  :commands disaster
  :defer    t
  :init
  (hook-fn 'c-mode-common-hook
    (local-set-key (kbd "C-c C-d") 'disaster)))

(use-package auto-complete-clang-async
  :commands ac-clang-launch-completion-process
  :init
  (hook-fn 'c-mode-common-hook
    (setq ac-sources '(ac-source-clang-async
                       ac-source-yasnippet
                       ac-source-words-in-buffer))
    (ac-clang-launch-completion-process))
  :config
  (progn
    (define-path cb:clang-complete-dir "lib/clang-complete-async/")
    (setq ac-clang-complete-executable (concat cb:clang-complete-dir "clang-complete"))))

(use-package c-eldoc
  :ensure   t
  :commands c-turn-on-eldoc-mode
  :init     (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(provide 'cb-clang)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-clang.el ends here
