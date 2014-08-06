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
(require 'utils-commands)

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
  :rem '("!")
  :custom '(("," . cb:comma-then-space)))

;;; Switch from repl to code.

(defun cb-js:switch-to-js ()
  "Switch from the repl to the last JS buffer."
  (interactive)
  (-if-let (buf (--first-buffer (derived-mode-p 'js2-mode)))
      (pop-to-buffer buf)
    (user-error "No JavaScript buffers")))

;;; Reindent braces on <return>

(defun cb-js:ret ()
  "Create a newline with appropriate formatting."
  (interactive "*")
  (let* ((sp (sp-get-enclosing-sexp))
         (was-at-braces? (equal "{" (plist-get sp :op)))
         (was-after-brace? (thing-at-point-looking-at (rx "{" (* space))))
         )
    (comment-indent-new-line)
    ;; Align closing brace.
    (when (and was-at-braces? was-after-brace?)
      (save-excursion
        (when (search-forward "}" (line-end-position) t)
          (forward-char -1)
          (comment-indent-new-line))))

    (indent-for-tab-command)))

;;; Use lambda symbol for anonymous functions.

(defvar cb-js:function-rx
  `((,(rx bow (group "function") eow (* space) "(")
     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                               ,(string-to-char "λ") 'decompose-region)
               nil)))))

(font-lock-add-keywords 'js2-mode cb-js:function-rx)
(font-lock-add-keywords 'skewer-repl-mode cb-js:function-rx)

;;; Key bindings

(after 'skewer-mode
  (define-key skewer-mode-map (kbd "C-c C-l") 'skewer-load-buffer)
  (define-key skewer-mode-map (kbd "RET") 'cb-js:ret)
  )

(after 'skewer-repl
  (define-key skewer-repl-mode-map (kbd "C-c C-z") 'cb-js:switch-to-js))

(provide 'config-javascript)

;;; config-javascript.el ends here
