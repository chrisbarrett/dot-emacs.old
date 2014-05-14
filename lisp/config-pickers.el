;;; config-pickers.el --- Configuration for various picker widgets.

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

;; Configuration for various picker widgets.

;;; Code:

(require 'utils-common)
(require 'utils-ui)

(define-command-picker sorting-picker
  :title "*Sorting*"
  :options
  '(("a" "Alpha" sort-lines)
    ("A" "Alpha (reverse)" (lambda () (sort-lines t (region-beginning) (region-end))))
    ("r" "Reverse" reverse-region)))

(defun cb:sort-dispatch ()
  "Open the appropriate sorting picker for the current mode."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (call-interactively 'org-sort))
   ((region-active-p)
    (call-interactively 'sorting-picker))
   (t
    (user-error "Sort commands require a region to be active"))))

(bind-key* "C-c ^" 'cb:sort-dispatch)

(define-command-picker viewing-picker
  :title "*Viewing*"
  :options
  '(("i" "IRC" show-irc)
    ("m" "Exposé (mode)" expose-buffers-by-mode)))

(bind-key* "C-c v" 'viewing-picker)

(define-command-picker help-picker
  :title "*Help Commands*"
  :options
  '(("m" "Messages" view-echo-area-messages)
    ("f" "Find Function" find-function)
    ("l" "Find Library" find-library)
    ("v" "Find Variable" find-variable)
    ("a" "Apropos" apropos)
    ("A" "Apropos (value)" apropos-value)))

(bind-key "C-h e" 'help-picker)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(define-command-picker narrowing-picker
  :title "*Narrowing*"
  :options
  '(("d" "Defun" narrow-to-defun :modes prog-mode)
    ("r" "Region" narrow-to-region :when region-active-p)
    ("w" "Widen" widen :when buffer-narrowed-p)
    ("b" "Block (org)" org-narrow-to-block :modes org-mode)
    ("e" "Element (org)" org-narrow-to-element :modes org-mode)
    ("s" "Subtree (org)" org-narrow-to-subtree :modes org-mode)))

(bind-key* "C-x n" 'narrowing-picker)

(bind-key* "C-x w" 'widen)

(provide 'config-pickers)

;;; config-pickers.el ends here
