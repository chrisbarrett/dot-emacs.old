;;; config-lisp.el --- Configure lisp

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

;; Configure lisp

;;; Code:

(require 'utils-common)
(require 'config-modegroups)

(hook-fn 'cb:lisp-modes-hook
  (local-set-key (kbd "M-q") 'indent-dwim))

(add-hook 'cb:lisp-modes-hook 'turn-on-eldoc-mode)

(hook-fn 'eldoc-mode-hook
  (diminish 'eldoc-mode))

(cb:install-package 'redshank)

(add-hook 'cb:lisp-modes-hook 'turn-on-redshank-mode)

(hook-fn 'redshank-mode-hook
  (diminish 'redshank-mode))

(provide 'config-lisp)

;;; config-lisp.el ends here
