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
(require 'noflet)
(require 'cb-mode-groups)

(use-package geiser
  :ensure t
  :commands run-geiser
  :init (hook-fn 'cb:scheme-modes (require 'geiser))
  :config
  (progn
    (after 'auto-complete
      (autoload 'geiser-company--prefix-at-point "geiser-company")
      (autoload 'geiser-company--doc "geiser-company")

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

    (defun geiser-eval-buffer ()
      "Evaluate the current Scheme buffer with Geiser."
      (interactive)
      ;; Switch to source if we're in the repl.
      (if (derived-mode-p 'repl-mode 'comint-mode 'inferior-scheme-mode)
          (save-excursion
            (switch-to-geiser)
            (geiser-eval-buffer)
            (switch-to-geiser))

        (let (result)
          (noflet ((message (&rest args) (setq result (apply 'format args))))
            (save-excursion
              (goto-char (point-min))
              (when (search-forward-regexp (rx bol "#lang" space (* nonl)) nil t)
                (forward-line)
                (beginning-of-line))
              (geiser-eval-region (point) (point-max))))

          (message "Buffer evaluated %s" result))))

    (after 'scheme
      (define-keys scheme-mode-map
        "C-c C-l" 'geiser-eval-buffer
        "C-c C-h" 'geiser-doc-look-up-manual))

    (defadvice switch-to-geiser (after append-with-evil activate)
      "Move to end of REPL and append-line."
      (when (derived-mode-p 'comint-mode)
        (cb:append-buffer)))

    (setq
     geiser-mode-start-repl-p t
     geiser-repl-startup-time 20000
     geiser-repl-history-filename (concat cb:tmp-dir "geiser-history")
     geiser-active-implementations '(racket))))

(provide 'cb-scheme)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-scheme.el ends here
