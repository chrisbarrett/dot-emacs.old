;;; cb-clojure.el --- Configuration for clojure

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0001

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

;; Configuration for clojure

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-mode-groups)
(require 'cb-evil)

;; `clojure-mode' provides a major mode for editing Clojure files.
(use-package clojure-mode
  :ensure t
  :commands (clojure-mode)
  :mode     ("\\.cljs?$" . clojure-mode)
  :config
  (progn

    (defun cb:switch-to-cider ()
      "Start cider or switch to an existing cider buffer."
      (interactive)
      (-if-let (buf (get-buffer "*cider*"))
          (cider-switch-to-repl-buffer buf)
        (cider-jack-in)))

    (hook-fn 'clojure-mode-hook
      (subword-mode +1)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-cider))))

;; `cider' provides a Clojure IDE and REPL for Emacs, built on top of nREPL.
(use-package cider
  :ensure   t
  :commands nrepl-jack-in
  :config
  (progn

    (defadvice cider-switch-to-repl-buffer (after insert-at-end-of-cider-line activate)
      "Enter insertion mode at the end of the line when switching to cider."
      (cb:append-buffer))

    (defadvice back-to-indentation (around move-to-cider-bol activate)
      "Move to position after prompt in cider."
      (if (equal major-mode 'cider-mode)
          (nrepl-bol)
        ad-do-it))

    (defun cb:switch-to-clojure ()
      "Switch to the last active clojure buffer."
      (interactive)
      (-when-let (buf (--first-buffer (derived-mode-p 'clojure-mode)))
        (pop-to-buffer buf)))

    (defun cb:eval-last-clj-buffer ()
      "Evaluate that last active clojure buffer without leaving the repl."
      (interactive)
      (-when-let (buf (--first-buffer (derived-mode-p 'clojure-mode)))
        (with-current-buffer buf
          (cider-eval-buffer))))

    (setq
     cider-popup-stacktraces    nil
     nrepl-hide-special-buffers t)

    (set-face-attribute 'cider-error-highlight-face t :inherit 'error)
    (set-face-underline 'cider-error-highlight-face nil)

    (after 'clojure-mode
      (define-key clojure-mode-map (kbd "C-c C-z") 'cb:switch-to-cider)
      (define-key clojure-mode-map (kbd "C-c C-h") 'cider-doc)
      (define-key clojure-mode-map (kbd "C-c C-f") 'cider-eval-buffer))

    (defadvice cider-popup-buffer-display (after set-mode activate)
      "Use `help-mode' as the major-mode for cider popup buffers."
      (with-current-buffer (ad-get-arg 0)
        (help-mode)))

    (hook-fns '(cider-mode-hook cider-repl-mode-hook)
      (cider-turn-on-eldoc-mode)
      (local-set-key (kbd "C-l") 'cider-repl-clear-buffer)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-clojure)
      (local-set-key (kbd "C-c C-f") 'cb:eval-last-clj-buffer))


    ;; Redefine doc handler so that the documentation buffer does not scroll as
    ;; new input is received.
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
                              nrepl-buffer-ns))))))

;; `ac-nrepl' provides auto-complete sources for Clojure using nrepl completions.
(use-package ac-nrepl
  :ensure t
  :commands
  (ac-nrepl-setup
   ac-nrepl-doc)
  :init
  (progn
    (add-hook 'cider-mode-hook 'ac-nrepl-setup)
    (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
    (after 'auto-complete
      (add-to-list 'ac-modes 'cider-mode)))
  :config
  (after 'cider
    (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)))

;; Enter insert mode in Clojure REPLs.
(after 'evil
  (hook-fn 'cider-repl-mode-hook (evil-insert-state nil)))

;; Add evil documentation lookup for Clojure.
(define-evil-doc-handler cb:clojure-modes (call-interactively 'cider-doc))

(provide 'cb-clojure)

;; Local Variables:
;; End:

;;; cb-clojure.el ends here
