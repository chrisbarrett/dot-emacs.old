;;; cb-dired.el --- Configuration for dired

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0008

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

;; Configuration for dired

;;; Code:

(require 'use-package)

(use-package dired
  :defer t
  :idle  (require 'dired)
  :init
  (hook-fn 'dired-mode-hook
    (evil-local-set-key 'normal (kbd "SPC") 'dired-hide-subdir)
    (evil-local-set-key 'normal (kbd "S-SPC") 'dired-hide-all)
    (local-set-key (kbd "M-N") 'dired-next-subdir)
    (local-set-key (kbd "M-P") 'dired-prev-subdir)
    (set (make-local-variable 'auto-revert-interval) 0.1)
    (set (make-local-variable 'auto-revert-verbose) nil)
    (auto-revert-mode +1))
  :config
  (progn
    (after 'hl-line

      (defun cb:line-is-dired-header? ()
        (equal 'dired-header
               (ignore-errors
                 (save-excursion
                   (move-to-column 3)
                   (face-at-point)))))

      (defadvice global-hl-line-highlight (around suppress-on-subdir-header activate)
        "Do not highlight the line if looking at a dired header."
        (if (and (derived-mode-p 'dired-mode) (cb:line-is-dired-header?))
            (global-hl-line-unhighlight)
          ad-do-it))

      (defadvice hl-line-highlight (around suppress-on-subdir-header activate)
        "Do not highlight the line if looking at a dired header."
        (if (and (derived-mode-p 'dired-mode) (cb:line-is-dired-header?))
            (hl-line-unhighlight)
          ad-do-it)))

    (setq dired-auto-revert-buffer t
          dired-listing-switches "-al --group-directories-first")
    (when (equal system-type 'darwin)
      ;; Use GNU version of ls if available.
      (-when-let (gls (executable-find "gls"))
        (setq ls-lisp-use-insert-directory-program t
              insert-directory-program gls)))))

(use-package dired-aux
  :defer t
  :init
  (progn
    (after 'dired (require 'dired-aux))
    (hook-fn 'dired-mode-hook
      (evil-local-set-key 'normal (kbd "TAB") 'dired-hide-subdir)
      (evil-local-set-key 'normal [backtab] 'dired-hide-all)
      (evil-local-set-key 'normal [backspace] 'dired-kill-subdir)))
  :config
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(use-package dired-x
  :commands
  (dired-jump
   dired-jump-other-window)
  :init
  (progn
    ;; Don't bind C-x C-j to dired-jump - this interferes with bindings in
    ;; ansi-term.
    (setq dired-bind-jump nil)
    (after 'dired (require 'dired-x))
    (bind-key* "M-d" 'dired-jump)
    (bind-key* "M-D" 'dired-jump-other-window)))

(use-package dired-details
  :ensure   t
  :commands dired-details-install
  :init     (after 'dired (dired-details-install))
  :config   (setq-default dired-details-hidden-string "… "))


(provide 'cb-dired)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-dired.el ends here
