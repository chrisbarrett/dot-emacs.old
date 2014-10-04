;;; config-org-babel.el --- Configuration for org-babel

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

;; Configuration for org-babel

;;; Code:

(require 'utils-common)
(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ledger . t)
   (C . t)
   (ditaa . t)
   (sh . t)
   (calc . t)
   (scala . t)
   (sqlite . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (ruby . t)
   (clojure . t)
   (haskell . t)))

(custom-set-variables
 '(org-src-fontify-natively t)
 '(org-confirm-babel-evaluate nil)
 '(org-edit-src-content-indentation 0))

(hook-fn 'org-src-mode-hook
  (setq-local require-final-newline nil))

(defvar org-edit-src-before-exit-hook nil
  "Hook run before exiting a code block.")

(defadvice org-edit-src-exit (before run-hook activate)
  "Run a hook when exiting src block."
  (run-hooks 'org-edit-src-before-exit-hook))

(add-hook 'org-edit-src-before-exit-hook 'delete-trailing-whitespace)

(provide 'config-org-babel)

;;; config-org-babel.el ends here
