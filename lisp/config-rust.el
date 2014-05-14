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


(provide 'config-rust)

;;; config-rust.el ends here
