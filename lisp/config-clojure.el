;;; config-clojure.el --- Configure clojure

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

;; Configure clojure

;;; Code:

(require 'utils-common)
(require 'utils-buffers)

(cb:declare-package-installer clojure
  :match (rx "." (or "clj" "edn" "dtm" "cljs" "cljx"))
  :packages (clojure-mode
             cider
             ac-nrepl))

(custom-set-variables
 '(cider-popup-stacktraces nil)
 '(nrepl-hide-special-buffers t))

(after 'evil
  (define-evil-doc-handler cb:clojure-modes (call-interactively 'cider-doc)))

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

(provide 'config-clojure)

;;; config-clojure.el ends here
