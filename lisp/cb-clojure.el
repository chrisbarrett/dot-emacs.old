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
(require 'cb-lib)

(use-package clojure-mode
  :ensure t
  :commands (clojure-mode)
  :mode     ("\\.cljs?$" . clojure-mode)
  :config
  (progn

    (defun cb:switch-to-cider ()
      "Start cider or switch to an existing cider buffer."
      (interactive)
      (-if-let (buf (get-buffer "*cider*"))
        (cider-switch-to-repl-buffer buf)
        (cider-jack-in)))

    (hook-fn 'clojure-mode-hook
      (subword-mode +1)
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-cider))))

(use-package cider
  :ensure   t
  :commands nrepl-jack-in
  :config
  (progn

    (defadvice cider-switch-to-repl-buffer (after insert-at-end-of-cider-line activate)
      "Enter insertion mode at the end of the line when switching to cider."
      (cb:append-buffer))

    (defadvice back-to-indentation (around move-to-cider-bol activate)
      "Move to position after prompt in cider."
      (if (equal major-mode 'cider-mode)
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
          (cider-eval-buffer))))

    (setq
     cider-popup-stacktraces    nil
     nrepl-hide-special-buffers t)

    (set-face-attribute 'cider-error-highlight-face t :inherit 'error)
    (set-face-underline 'cider-error-highlight-face nil)

    (hook-fn 'clojure-mode-hook
      (local-set-key (kbd "C-c C-z") 'cb:switch-to-cider)
      (local-set-key (kbd "C-c C-h") 'cider-doc)
      (local-set-key (kbd "C-c C-f") 'cider-eval-buffer))

    (hook-fns '(cider-mode-hook cider-interaction-mode-hook)
      (cider-turn-on-eldoc-mode)
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
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-interaction-mode-hook 'ac-cider-setup)
    (after 'auto-complete
      (add-to-list 'ac-modes 'cider-mode)))
  :config
  (after 'cider
    (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)))

(provide 'cb-clojure)

;; Local Variables:
;; End:

;;; cb-clojure.el ends here
