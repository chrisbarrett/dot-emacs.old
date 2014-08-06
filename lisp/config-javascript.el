;;; config-javascript.el --- Configuration for JavaScript.

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

;; Configuration for JavaScript.

;;; Code:

(require 'utils-common)

(cb:declare-package-installer javascript
  :match (rx ".js" eos)
  :packages (js2-mode skewer-mode))

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

(add-hook 'js2-mode-hook 'skewer-mode)

(hook-fn 'css-mode-hook
  (cb:install-package 'skewer-mode)
  (skewer-css-mode))

(hook-fn 'html-mode-hook
  (cb:install-package 'skewer-mode)
  (skewer-html-mode))

;;; Smart operators

(super-smart-ops-configure-for-mode 'js2-mode
  :custom '(("," . cb:comma-then-space)))

;;; Switch from repl to code.

(defun cb-js:switch-to-js ()
  "Switch from the repl to the last JS buffer."
  (interactive)
  (-if-let (buf (--first-buffer (derived-mode-p 'js2-mode)))
      (pop-to-buffer buf)
    (user-error "No JavaScript buffers")))

(after 'skewer-repl
  (define-key skewer-repl-mode-map (kbd "C-c C-z") 'cb-js:switch-to-js))

(provide 'config-javascript)

;;; config-javascript.el ends here
