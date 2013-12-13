;;; cb-eshell.el --- Configuration for eshell

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130915.1146

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

;; Configuration for eshell

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-colour)

(defvar-local cb-eshell:last-header nil)

;; `eshell' is a terminal emulator written in elisp.
(use-package eshell
  :commands eshell
  :bind ("<f1>" . cb:term-cycle)
  :config
  (progn
    (setenv "EDITOR" "emacsclient")

    (setq
     ;; Case-insensitive filename completion.
     eshell-cmpl-ignore-case t
     ;; Use custom faces.
     eshell-highlight-prompt nil
     eshell-prompt-regexp (rx bol (* space) (or "#" ":") space)
     eshell-prompt-function 'cb-eshell:format-prompt)

    (defun cb:term-cycle (&optional arg)
      "Cycle through various terminal window states."
      (interactive)
      (cond

       ;; If terminal is maximized, restore previous window config.
       ((and (derived-mode-p 'eshell-mode)
             (equal 1 (length (window-list)))
             (equal (buffer-name) "*eshell*"))
        (or (ignore-errors (jump-to-register :term-fullscreen) t)
            (bury-buffer)))

       ;; If we're looking at the terminal, maximise it.
       ((derived-mode-p 'eshell-mode)
        (delete-other-windows))

       ;; Otherwise show the terminal.
       (t
        ;; Hide the term window if it's visible.
        (-when-let (win (--first (equal (buffer-name (window-buffer it))
                                        "*eshell*")
                                 (window-list)))
          (delete-window win))
        ;; Save this configuration to a register so that it can be restored
        ;; for later positions in the cycle.
        (window-configuration-to-register :term-fullscreen)
        ;; Show terminal.
        (eshell arg))))

    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        ;; Delete all but the last line of prompt.
        (delete-region (point-min)
                       (save-excursion
                         (goto-char (point-max))
                         (search-backward-regexp eshell-prompt-regexp)
                         (line-beginning-position)))))

    (defun eshell-subshelling-ret ()
      (interactive)
      (end-of-line)
      (unless (s-matches? (rx "&" (* space)) (current-line))
        (just-one-space)
        (insert "&")
        (eshell-send-input)))

    (hook-fn 'eshell-mode-hook
      (local-set-key (kbd "M->") 'cb:append-buffer)
      (local-set-key (kbd "C-l") 'eshell/clear)
      (local-set-key (kbd "C-<return>") 'eshell-subshelling-ret))

    ;; Configure the eshell prompt.
    ;;
    ;; Displays the current working directory only when it changes.

    (defface eshell-prompt-sep
      '((t :inherit 'font-lock-comment-face))
      "Face for separators in the eshell prompt."
      :group 'cb-eshell)

    (defun cb-eshell:current-dir ()
      (let* ((cwd (f-short (eshell/pwd)))
             (colour (if (s-starts-with? "/" cwd)
                         solarized-hl-orange
                       solarized-hl-cyan)))
        (propertize cwd 'face `(:foreground ,colour))))

    (defun cb-eshell:git-status ()
      (when (and (executable-find "git")
                 (locate-dominating-file (eshell/pwd) ".git"))
        (concat
         (propertize " | " 'face 'eshell-prompt-sep)
         ;; Branch
         (propertize (%-string "git rev-parse --abbrev-ref HEAD")
                     'face `(:foreground ,solarized-hl-yellow))
         ;; @
         (propertize "@" 'face 'eshell-prompt-sep)
         ;; Rev
         (substring (%-string "git rev-parse HEAD") 0 7)
         ;; State

         (let ((s (concat (when (magit-anything-unstaged-p)
                            (propertize "M" 'face `(:foreground ,solarized-hl-orange)))
                          (when (magit-anything-staged-p)
                            (propertize "+" 'face `(:foreground ,solarized-hl-green))))))
           (unless (s-blank? s)
             (concat " " s))))))

    (defun cb-eshell:prompt-symbol ()
      (let ((ch (if (= (user-uid) 0) "#" ":"))
            (colour (if (zerop eshell-last-command-status)
                        solarized-hl-cyan
                      solarized-hl-red)))
        (propertize ch 'face (list :foreground colour))))

    (defun cb-eshell:format-header ()
      (concat
       (when cb-eshell:last-header "\n")
       (propertize " [ " 'face 'eshell-prompt-sep)
       (cb-eshell:current-dir)
       (cb-eshell:git-status)
       (propertize " ]" 'face 'eshell-prompt-sep)
       "\n"))

    (defun cb-eshell:format-prompt ()
      "Format the prompt to display in eshell."
      (let* ((h (cb-eshell:format-header))
             (changed? (not (equal h cb-eshell:last-header)))
             (p (concat (when changed? h) " " (cb-eshell:prompt-symbol) " ")))
        (setq cb-eshell:last-header h)
        (propertize p 'read-only t 'front-sticky 'read-only 'rear-nonsticky 'read-only)))))

(after 'smartparens
  (add-hook 'eshell-mode-hook 'smartparens-mode))

(provide 'cb-eshell)

;; Local Variables:
;; End:

;;; cb-eshell.el ends here
