;;; cb-scheme.el --- Configuration for scheme

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0000

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

;; Configuration for scheme

;;; Code:

(require 'use-package)
(require 'cb-mode-groups)
(autoload 'geiser-company--doc "geiser-company")
(autoload 'geiser-company--prefix-at-point "geiser-company")
(autoload 'popwin:popup-buffer "popwin")

;; `geiser' provides slime-like interaction for Scheme.  I mainly use Racket, so
;; the config below probably doesn't work for other Schemes.
(use-package geiser
  :ensure t
  :commands run-geiser
  :init (hook-fn 'cb:scheme-modes (require 'geiser))
  :config
  (progn

    (setq geiser-mode-start-repl-p t
          geiser-repl-startup-time 20000
          geiser-repl-history-filename (concat cb:tmp-dir "geiser-history")
          geiser-active-implementations '(racket))

    (after 'scheme
      (define-keys scheme-mode-map
        "C-c C-l" 'geiser-eval-buffer
        "C-c C-h" 'geiser-doc-look-up-manual))

    ;; Auto-complete
    ;;
    ;; Geiser supports company-mode. Adapt to auto-complete.
    (after 'auto-complete
      (defun cb:geiser-ac-doc (fname &optional module impl)
        (let* ((symbol (intern fname))
               (impl (or impl geiser-impl--implementation))
               (module (geiser-doc--module (or module (geiser-eval--get-module))
                                           impl)))
          (-when-let (ds (geiser-doc--get-docstring symbol module))
            (ignore-errors
              (with-temp-buffer
                (geiser-doc--insert-title
                 (geiser-autodoc--str* (cdr (assoc "signature" ds))))
                (newline)
                (insert (or (cdr (assoc "docstring" ds)) ""))
                (buffer-string))))))

      (ac-define-source geiser
        '((candidates . (progn
                          (geiser-company--prefix-at-point)
                          (cdr geiser-company--completions)))
          (document   . cb:geiser-ac-doc)))

      (hook-fn 'cb:scheme-modes-hook
        (setq ac-sources '(ac-source-yasnippet ac-source-geiser))))

    ;; Override behaviours

    (after 'geiser-mode
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
                                     nomsg))))

    (defadvice switch-to-geiser (after append-with-evil activate)
      "Move to end of REPL and append-line."
      (when (derived-mode-p 'comint-mode)
        (cb:append-buffer)))))

;; Provide a command to compile and run the current buffer on C-c C-c
(after 'scheme
  ;; String
  (defconst cbscm:run-scm-bufname "*execute scheme*")

  ;; String -> String
  (defun cbscm:lang (s)
    (cadr (s-match (rx bol "#lang" (+ space) (group (+ nonl))) s)))

  ;; FilePath -> IO Process
  (defun cbscm:run-file (file)
    (interactive "f")
    (async-shell-command
     (s-join " "
             `("racket"
               ,@(-when-let (lang (cbscm:lang (f-read-text file)))
                   (list "-I" lang))
               ,file))
     cbscm:run-scm-bufname))

  ;; IO ()
  (defun cbscm:execute-buffer ()
    "Compile and run the current buffer in Racket."
    (interactive)
    ;; Offer to save buffer.
    (when (and (buffer-modified-p) (y-or-n-p "Save buffer? "))
      (save-buffer))
    (if (and (buffer-file-name) (f-exists? (buffer-file-name)))
        (cbscm:run-file (buffer-file-name))
      (let ((f (make-temp-file nil nil ".rkt")))
        (f-write (buffer-string) 'utf-8 f)
        (cbscm:run-file f))))

  (define-key scheme-mode-map (kbd "C-c C-c") 'cbscm:execute-buffer))

(provide 'cb-scheme)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-scheme.el ends here
