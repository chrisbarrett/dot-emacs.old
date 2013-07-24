;;; cb-clojure.el --- Configuration for clojure

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0001

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

;; Configuration for clojure

;;; Code:

(require 'use-package)

(use-package clojure-mode
  :ensure t
  :commands (clojure-mode)
  :mode     ("\\.cljs?$" . clojure-mode)
  :config
  (progn

    (defun cb:switch-to-nrepl ()
      "Start nrepl or switch to an existing nrepl buffer."
      (interactive)
      (-if-let (buf (get-buffer "*nrepl*"))
        (nrepl-switch-to-repl-buffer buf)
        (nrepl-jack-in)))

    (hook-fn 'clojure-mode-hook
      (subword-mode +1)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl))))

(use-package nrepl
  :ensure   t
  :commands nrepl-jack-in
  :config
  (progn

    (defadvice nrepl-switch-to-repl-buffer (after insert-at-end-of-nrepl-line activate)
      "Enter insertion mode at the end of the line when switching to nrepl."
      (cb:append-buffer))

    (defadvice back-to-indentation (around move-to-nrepl-bol activate)
      "Move to position after prompt in nREPL."
      (if (equal major-mode 'nrepl-mode)
          (nrepl-bol)
        ad-do-it))

    (defun cb:switch-to-clojure ()
      "Switch to the last active clojure buffer."
      (interactive)
      (-when-let (buf (--first-buffer (derived-mode-p 'clojure-mode)))
        (pop-to-buffer buf)))

    (defun cb:eval-last-clj-buffer ()
      "Evaluate that last active clojure buffer without leaving the repl."
      (interactive)
      (-when-let (buf (--first-buffer (derived-mode-p 'clojure-mode)))
        (with-current-buffer buf
          (nrepl-eval-buffer))))

    (setq
     nrepl-popup-stacktraces    nil
     nrepl-hide-special-buffers t)

    (set-face-attribute 'nrepl-error-highlight-face t :inherit 'error)
    (set-face-underline 'nrepl-error-highlight-face nil)

    (hook-fn 'clojure-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl)
      (local-set-key (kbd "C-c C-h") 'nrepl-doc)
      (local-set-key (kbd "C-c C-f") 'nrepl-eval-buffer))

    (hook-fns '(nrepl-mode-hook nrepl-interaction-mode-hook)
      (nrepl-turn-on-eldoc-mode)
      (subword-mode +1)
      (local-set-key (kbd "C-l") 'nrepl-clear-buffer)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-clojure)
      (local-set-key (kbd "C-c C-f") 'cb:eval-last-clj-buffer))))

(use-package ac-nrepl
  :ensure t
  :commands
  (ac-nrepl-setup
   ac-nrepl-doc)
  :init
  (progn
    (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
    (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
    (after 'auto-complete
      (add-to-list 'ac-modes 'nrepl-mode)))
  :config
  (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(use-package midje-mode
  :ensure   t
  :commands midje-mode
  :diminish midje-mode
  :init    (add-hook 'clojure-mode-hook 'midje-mode))

(provide 'cb-clojure)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-clojure.el ends here
