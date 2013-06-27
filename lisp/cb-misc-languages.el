;;; cb-misc-languages.el --- Configuration for miscellaneous language modes.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130627.0019

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

;; Configuration for miscellaneous language modes.

;;; Code:

(require 'use-package)

(use-package json-mode
  :ensure    t
  :commands
  (json-mode
   beautify-json)
  :mode      ("\\.json$" . json-mode)
  :init      (defalias 'format-json 'beautify-json)
  :config    (add-to-list 'ac-modes 'json-mode))

(use-package fsharp-mode
  :ensure   t
  :commands fsharp-mode
  :mode ("\\.fs[ixly]?$" . fsharp-mode)
  :config
  (progn
    (add-to-list 'ac-modes 'fsharp-mode)
    (unless (display-graphic-p)
      (setq fsharp-ac-use-popup nil))
    (add-hook 'fsharp-mode-hook 'electric-indent-mode)
    (add-hook 'fsharp-mode-hook 'electric-layout-mode)))

(use-package conf-mode
  :mode
  ((".gitignore$"  . conf-mode)
   (".gitmodules$" . conf-mode)
   ("ackrc$"       . conf-mode)
   ("Doxyfile$"    . conf-mode))
  :init
  (hook-fn 'cb:conf-modes-hook
    (smartparens-mode +1)))

(use-package sml-mode
  :ensure t
  :mode (("\\.cm"  . sml-cm-mode)
         ("\\.sml" . sml-mode)
         ("\\.sig" . sml-mode)
         ("\\.grm" . sml-yacc-mode))
  :init
  (--each '(".cm/" "CM/")
    (add-to-list 'completion-ignored-extensions it))
  :config
  (setq
   sml-indent-level 2))

(provide 'cb-misc-languages)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-misc-languages.el ends here
