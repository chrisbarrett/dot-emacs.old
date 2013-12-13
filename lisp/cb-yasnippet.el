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

(defun cbyas:current-field ()
  "Return the current active field."
  (and (boundp 'yas--active-field-overlay)
       yas--active-field-overlay
       (overlay-buffer yas--active-field-overlay)
       (overlay-get yas--active-field-overlay 'yas--field)))

(defun cbyas:beginning-of-field ()
  (-when-let (field (cbyas:current-field))
    (marker-position (yas--field-start field))))

(defun cbyas:end-of-field ()
  (-when-let (field (cbyas:current-field))
    (marker-position (yas--field-end field))))

(defun cbyas:current-field-text ()
  "Return the text in the active snippet field."
  (-when-let (field (cbyas:current-field))
    (yas--field-text-for-display field)))

(use-package yasnippet
  :ensure t
  :if (not noninteractive)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :diminish yas-minor-mode
  :init
  (progn

    (defface yas-field-highlight-face
      `((t :underline ,solarized-hl-cyan
           :italic t))
      "The face used to highlight the currently active field of a snippet"
      :group 'yasnippet)

    ;; Don't use yasnippet's default snippets, which are mostly crappy.
    (defvar yas-snippet-dirs (list cb:yasnippet-dir))

    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'text-mode-hook 'yas-minor-mode))

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

    (bind-key* "C-c y" 'yasnippet-picker)

    ;; Commands

    (defun cbyas:backspace ()
      "Clear the current field if the current snippet is unmodified.
Otherwise delete backwards."
      (interactive "*")
      (let ((field (cbyas:current-field)))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              ((true? smartparens-mode)
               (call-interactively 'sp-backward-delete-char))
              (t
               (call-interactively 'backward-delete-char)))))

    (defun cbyas:space ()
      "Clear and skip this field if it is unmodified. Otherwise insert a space."
      (interactive "*")
      (let ((field (cbyas:current-field)))
        (cond ((and field
                    (not (yas--field-modified-p field))
                    (eq (point) (marker-position (yas--field-start field))))
               (yas--skip-and-clear field)
               (yas-next-field 1))
              (t
               (insert " ")))))

    (bind-keys
      :map yas-keymap
      "<backspace>" 'cbyas:backspace
      "SPC"         'cbyas:space)

    ;; Advice

    (defun cbyas:clear-blank-field ()
      "Clear the current field if it is blank."
      (-when-let* ((beg (cbyas:beginning-of-field))
                   (end (cbyas:end-of-field))
                   (str (cbyas:current-field-text)))
        (when (s-matches? (rx bos (+ space) eos) str)
          (delete-region beg end)
          t)))

    (defun cbyas:maybe-goto-field-end ()
      "Move to the end of the current field if it has been modified."
      (-when-let (field (cbyas:current-field))
        (when (and (yas--field-modified-p field)
                   (yas--field-contains-point-p field))
          (goto-char (cbyas:end-of-field)))))

    (defadvice yas-next-field (before clear-blank-field activate)
      (cbyas:clear-blank-field))

    (defadvice yas-prev-field (before clear-blank-field activate)
      (cbyas:clear-blank-field))

    (defadvice yas-next-field (after goto-field-end activate)
      (cbyas:maybe-goto-field-end))

    (defadvice yas-prev-field (after goto-field-end activate)
      (cbyas:maybe-goto-field-end))))

(after 'evil

  (defadvice yas-prev-field (after insert-state activate)
    "Enter evil insert state."
    (when (true? evil-mode)
      (evil-insert-state))

    (defadvice yas-prev-field (after insert-state activate)
      "Enter evil insert state."
      (when (true? evil-mode)
        (evil-insert-state)))

    (add-hook 'yas-before-expand-snippet-hook 'evil-insert-state)))

(provide 'cb-yasnippet)

;; Local Variables:
;; End:

;;; cb-yasnippet.el ends here
