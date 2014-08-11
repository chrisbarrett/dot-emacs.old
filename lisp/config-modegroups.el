;;; config-modegroups.el --- Define mode groups

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

;; Define mode groups

;;; Code:

(require 'utils-common)

(defmacro define-combined-hook (name hooks)
  "Create a hook bound as NAME that is run after each hook in HOOKS."
  (declare (indent 1))
  `(progn
     (defvar ,name nil "Auto-generated combined hook.")
     (hook-fns ',(eval hooks)
       (run-hooks ',name))))

(defmacro define-mode-group (name modes)
  "Create an ad-hoc relationship between language modes.
Creates a special var with NAME to contain the grouping.
Declares a hook NAME-hook that runs after any of MODES are initialized."
  (declare (indent 1))
  (let ((hook (intern (format "%s-hook" name))))
    `(progn
       ;; Define modes variable.
       (defconst ,name ,modes "Auto-generated variable for language grouping.")
       ;; Create a combined hook for MODES.
       (define-combined-hook ,hook
         (--map (intern (concat (symbol-name it) "-hook"))
                ,modes)))))

(define-mode-group cb:scheme-modes
  '(scheme-mode
    inferior-scheme-mode
    geiser-repl-mode))

(define-mode-group cb:clojure-modes
  '(clojure-mode
    clojurescript-mode
    cider-repl-mode))

(define-mode-group cb:elisp-modes
  '(emacs-lisp-mode
    inferior-emacs-lisp-mode))

(define-mode-group cb:lisp-modes
  `(cider-repl-mode
    clojure-mode
    clojurescript-mode
    common-lisp-mode
    emacs-lisp-mode
    geiser-repl-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    inferior-scheme-mode
    lisp-mode
    repl-mode
    scheme-mode
    slime-mode
    slime-repl-mode))

(define-mode-group cb:haskell-modes
  '(haskell-mode
    inferior-haskell-mode
    haskell-interactive-mode
    haskell-c-mode
    haskell-cabal-mode))

(define-mode-group cb:idris-modes
  '(idris-mode
    idris-repl-mode))

(define-mode-group cb:python-modes
  '(python-mode
    inferior-python-mode))

(define-mode-group cb:ruby-modes
  '(inf-ruby-mode
    ruby-mode))

(define-mode-group cb:xml-modes
  '(sgml-mode
    nxml-mode))

(define-mode-group cb:org-minor-modes
  '(orgtbl-mode
    org-indent-mode
    orgstruct-mode
    orgstruct++-mode))

(define-mode-group cb:prompt-modes
  '(comint-mode
    inf-ruby-mode
    inferior-python-mode
    ielm-mode
    erc-mode
    utop-mode
    slime-repl-mode
    inferior-scheme-mode
    inferior-haskell-mode
    sclang-post-buffer-mode))

(define-mode-group cb:whitespace-sensitive-languages
  '(python-mode
    haskell-mode
    fsharp-mode
    idris-mode))


(provide 'config-modegroups)

;;; config-modegroups.el ends here
