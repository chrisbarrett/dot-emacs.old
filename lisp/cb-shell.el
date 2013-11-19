;;; cb-shell.el --- Configuration for shells

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0017

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

;; Configuration for shells

;;; Code:

(require 'use-package)

(use-package term
  :defer t
  :config
  (progn
    (add-to-list 'ac-modes 'term-mode)

    (defun cb:ansi-term-paste ()
      "Correct paste handling in ansi-term."
      (interactive)
      (process-send-string
       (get-buffer-process (current-buffer))
       (current-kill 0)))

    (hook-fn 'term-mode-hook
      (setq ac-sources '(ac-source-filename))

      (local-set-key (kbd "s-v") 'cb:ansi-term-paste)

      (when (true? evil-mode)
        (evil-define-key 'normal term-mode-map (kbd "p") 'cb:ansi-term-paste))

      (define-key term-raw-map (kbd "M-T") 'cb:term-cycle)
      ;; Yasnippet causes tab-completion to fail.
      (yas-minor-mode -1))

    (hook-fn 'window-configuration-change-hook
      "Change process window size."
      (when (and (derived-mode-p 'comint-mode 'term-mode)
                 (get-buffer-process (current-buffer)))
        (set-process-window-size (get-buffer-process (current-buffer))
                                 (window-height)
                                 (window-width))))

    (defadvice ansi-term (after move-to-end-of-buffer activate)
      "Move to the end of the shell buffer and enter insertion state."
      (cb:append-buffer))))

(use-package sh-script
  :mode (("\\.zsh" . shell-script-mode )))

(provide 'cb-shell)

;; Local Variables:
;; End:

;;; cb-shell.el ends here
