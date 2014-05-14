;;; config-lambda-mode.el --- Configure lambda-mode

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

;; Configure lambda-mode

;;; Code:

(require 'lambda-mode)

(custom-set-variables
 '(lambda-symbol (string (make-char 'greek-iso8859-7 107))))

(add-hook 'cb:scheme-modes-hook    'lambda-mode)
(add-hook 'inferior-lisp-mode-hook 'lambda-mode)
(add-hook 'lisp-mode-hook          'lambda-mode)
(add-hook 'cb:elisp-modes-hook     'lambda-mode)
(add-hook 'cb:python-modes-hook    'lambda-mode)
(add-hook 'cb:slime-modes-hook     'lambda-mode)

(hook-fn 'lambda-mode-hook
  (diminish 'lambda-mode))


(provide 'config-lambda-mode)

;;; config-lambda-mode.el ends here
