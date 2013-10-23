;;; cb-spelling.el --- Configuration for ispell.

;; Copyright (C) 2013 Chris Barrett

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

;; Configuration for ispell.

;;; Code:

(require 'cb-lib)
(require 'use-package)
(require 'ispell)

(defun ispell-add-to-dict (word)
  "Add WORD to the user's dictionary."
  (ispell-send-string (concat "*" word "\n"))
  (setq ispell-pdict-modified-p '(t))
  (ispell-pdict-save ispell-silently-savep))

(use-package ispell
  :config
  (setq ispell-program-name "aspell"
        ispell-dictionary "en_GB"
        ispell-silently-savep t))

(use-package flyspell
  :diminish flyspell-mode
  :defer t
  :init (unless noninteractive (require 'flyspell))
  :config
  (progn

    (setq flyspell-delay 1)

    (define-key flyspell-mouse-map [down-mouse-3] 'flyspell-correct-word)
    (define-key flyspell-mouse-map [mouse-3] 'undefined)

    (hook-fn 'after-init-hook
      "Enable flyspell after Emacs has started up."
      (add-hook 'text-mode-hook 'flyspell-mode)
      (add-hook 'prog-mode-hook 'flyspell-prog-mode)
      (hook-fn 'cb:xml-modes-hook
        (unless (derived-mode-p 'markdown-mode)
          (flyspell-prog-mode))))))

(use-package flyspell-lazy
  :ensure  t
  :defer   t
  :init (add-hook 'flyspell-mode-hook 'flyspell-lazy-mode))

(provide 'cb-spelling)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-spelling.el ends here
