;;; config-whitespace.el --- Configue whitespace

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

;; Configue whitespace

;;; Code:

(require 'utils-common)

(require 'whitespace)

(custom-set-variables
 '(whitespace-line-column 80)
 '(whitespace-style '(face lines-tail)))

(diminish 'whitespace-mode)

;;; Enable `whitespace-mode' for most programming modes.

(defun cb-ws:set-whitespace-mode ()
   "Conditionally enable whitespace mode.
In particular, disable for org src blocks so ws highlighting is not exported."
   (if (or (true? org-src-mode)
           (derived-mode-p 'haskell-mode)) ; Long lines are OK in Haskell
       (whitespace-mode +1)
     (whitespace-mode -1)))

(add-hook 'prog-mode-hook 'cb-ws:set-whitespace-mode)

(defadvice whitespace-turn-on (around ignore-errors activate)
  "Ignore void-function errors when starting whitespace mode."
  (condition-case _
      ad-do-it
    (void-function)))

(provide 'config-whitespace)

;;; config-whitespace.el ends here
