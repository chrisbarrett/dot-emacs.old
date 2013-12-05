;;; cb-yasnippet.el --- Configuration for yasnippet

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130805.0332

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

;; Configuration for yasnippet

;;; Code:

(require 'use-package)
(require 'cb-lib)
(require 'cb-foundation)
(require 'cb-colour)

(defun cbyas:bol? ()
  "Non-nil if point is on an empty line or at the first word."
  (s-matches? (rx bol (* space) (* word) eol)
              (buffer-substring (line-beginning-position)
                                (point))))

(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :diminish yas-minor-mode
  :init
  (progn

    (defface yas-field-highlight-face
      `((t (:box (:line-width -1 :color ,solarized-hl-cyan))))
      "The face used to highlight the currently active field of a snippet"
      :group 'yasnippet)

    ;; Don't use yasnippet's default snippets, which are mostly crappy.
    (defvar yas-snippet-dirs (list cb:yasnippet-dir))

    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'text-mode-hook 'yas-minor-mode)

    ;; Advice

    (defun cbyas:yas-face? ()
      "Non-nil if point is at a yasnippet face."
      (-contains? '(yas--field-debug-face yas-field-highlight-face)
                  (face-at-point)))

    (defun cbyas:beginning-of-field ()
      (let (beg)
        (save-excursion
          (while (and (not (eobp)) (cbyas:yas-face?))
            (setq beg (1+ (point)))
            (forward-char -1)))
        beg))

    (defun cbyas:end-of-field ()
      (let (end)
        (save-excursion
          (while (and (not (bobp)) (cbyas:yas-face?))
            (setq end (point))
            (forward-char 1)))
        end))

    (defun cbyas:current-field-text ()
      "Return the text in the active snippet field."
      (-when-let* ((beg (cbyas:beginning-of-field))
                   (end (cbyas:end-of-field)))
        (buffer-substring beg end)))

    (defun cbyas:clear-blank-field ()
      "Clear the current field if it is blank."
      (-when-let* ((beg (cbyas:beginning-of-field))
                   (end (cbyas:end-of-field))
                   (str (cbyas:current-field-text)))
        (when (s-matches? (rx bos (* space) eos) str)
          (delete-region beg end)
          t)))

    (defadvice yas-next-field (before clear-blank-field activate)
      (cbyas:clear-blank-field))

    (defadvice yas-prev-field (before clear-blank-field activate)
      (cbyas:clear-blank-field))

    (defadvice yas-prev-field (after desc activate)
      (-when-let (end (cbyas:end-of-field))
        (goto-char end))))

  :config
  (progn

    (setq yas-prompt-functions '(yas-ido-prompt)
          yas-wrap-around-region t)

    (yas-global-mode t)
    (hook-fn 'snippet-mode-hook
      (setq require-final-newline nil))

    (defun cbyas:reload-all ()
      (interactive)
      (yas-recompile-all)
      (yas-reload-all))

    (define-command-picker yasnippet-picker
      :title "*Yasnippet Commands*"
      :options
      '(("e" "Expand" yas-expand)
        ("f" "Visit File" yas-visit-snippet-file)
        ("i" "Insert" yas-insert-snippet)
        ("n" "New" yas-new-snippet)
        ("r" "Reload All" cbyas:reload-all)
        ("t" "Show Tables" yas-describe-tables)))

    (bind-key* "C-c y" 'yasnippet-picker)))

(after 'evil

  (defadvice yas-prev-field (after insert-state activate)
    "Enter evil insert state."
    (when (true? evil-mode)
      (evil-insert-state)))

  (add-hook 'yas-before-expand-snippet-hook 'evil-insert-state))

(provide 'cb-yasnippet)

;; Local Variables:
;; End:

;;; cb-yasnippet.el ends here
