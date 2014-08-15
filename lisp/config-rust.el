;;; config-rust.el --- Configure Rust

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

;; Configure Rust

;;; Code:

(require 'utils-common)
(require 'super-smart-ops)

(cb:declare-package-installer rust
  :match (rx ".rs" eol)
  :packages (rust-mode flycheck-rust))

(after '(rust-mode flycheck) (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

;;; Smart operators

(defun cbrs:smart-colon ()
  "Insert a colon as a smart operator.
Collapse spaces if this is a double-colon."
  (interactive "*")
  (super-smart-ops-insert ":")
  (save-excursion
    (when (search-backward-regexp (rx (* space) ":" (* space) ":" (* space))
                                  nil t)
      (replace-match "::")
      (search-backward "::")
      (delete-horizontal-space))))

(super-smart-ops-configure-for-mode 'rust-mode
  :rem '("!" "~" "&")
  :custom '((":" . cbrs:smart-colon)
            ("," . cb:comma-then-space)))

(defun cbrs:insert-type-brackets ()
  (interactive)
  (save-restriction
    (narrow-to-region (line-beginning-position) (point))
    (delete-horizontal-space)
    (insert "<>")
    (forward-char -1)))

;;; Flycheck

(defun cbrs:set-rust-library-path ()
  "Set the search path for rust libraries."
  (require 'flycheck)
  (add-to-list 'flycheck-rust-library-path ".")
  (when (projectile-project-p)
    (add-to-list 'flycheck-rust-library-path (f-join (projectile-project-root) "src"))
    (add-to-list 'flycheck-rust-library-path (f-join (projectile-project-root) "lib"))))

(add-hook 'rust-mode-hook 'cbrs:set-rust-library-path)

(put 'rust :flycheck-command
     '("rustc" "--crate-type" "lib" "--no-trans"
       (option-list "-L" flycheck-rust-library-path s-prepend)
       source-inplace))

;;; Snippet utilities

(defun cbrs:bol-or-after-accessibility-modifier? ()
  "Predicate for snippets"
  (save-excursion
    (save-restriction
      ;; Move past access modifier.
      (goto-char (line-beginning-position))
      (search-forward-regexp (rx bow "pub" eow (* space)) (line-end-position) t)
      (narrow-to-region (point) (line-end-position))
      (cbyas:bol?))))

(defun cbrs:fmt-println-args (text)
  "Format the contents of a call to `println!' based on the given format string."
  (let ((n (s-count-matches "{" text)))
    (s-repeat n ", ")))

(defun cbrs:previous-struct-def ()
  "Search backward for the name of the last struct defined in this file."
  (save-match-data
    (if (search-backward-regexp (rx (or "enum" "struct") (+ space)
                                    (group (+ (not (any "{")))))
                                nil t)
        (s-trim (match-string 1))
      "Name")))

;;; Company

;; (after 'rust-mode
;;   (require 'racer)
;;   (add-to-list 'company-backends '(racer-company-complete
;;                                    :with company-dabbrev)))

(defvar racer-cmd (f-join cb:lib-dir "racer" "bin" "racer"))
(defvar rust-srcpath (f-join user-home-directory "src" "rust" "src"))

;; HACK: Override company idle delay set by racer.
(hook-fn 'rust-mode-hook
  (run-with-timer 0.1 nil (lambda () (setq-local company-idle-delay 0.1))))

;;; Key bindings

(after 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-,") 'cbrs:insert-type-brackets)
  )

(provide 'config-rust)

;;; config-rust.el ends here
