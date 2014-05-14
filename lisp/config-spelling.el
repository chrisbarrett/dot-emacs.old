;;; config-spelling.el --- Configuration for ispell and flyspell

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

;; Configuration for ispell and flyspell

;;; Code:

(require 'utils-common)

(autoload 'ispell-send-string "ispell")

(unless noninteractive
  (require 'ispell))

(setq ispell-program-name "aspell")

(setq ispell-dictionary "en_GB")

(setq ispell-silently-savep t)

(defun ispell-add-to-dict (word)
  "Add WORD to the user's dictionary."
  (ispell-send-string (concat "*" word "\n"))
  (setq ispell-pdict-modified-p '(t))
  (ispell-pdict-save ispell-silently-savep))

(unless noninteractive (require 'flyspell))

(setq flyspell-delay 1)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'cb:xml-modes-hook 'flyspell-prog-mode)

(after 'flyspell
  (define-key flyspell-mouse-map [down-mouse-3] 'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] 'undefined)
  (define-key flyspell-mode-map (kbd "C-c $") nil))

(hook-fn 'flyspell-mode-hook
  (diminish 'flyspell-mode))

(cb:install-package 'flyspell-lazy)
(add-hook 'flyspell-mode-hook 'flyspell-lazy-mode)

(defadvice flyspell-lazy-check-pending (around ignore-errors activate)
  "Ignore errors when checking."
  (ignore-errors
    (save-restriction
      (widen)
      ad-do-it)))

(provide 'config-spelling)

;;; config-spelling.el ends here
