;;; cb-org.el --- Configuration for org

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0014

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

;; Configuration for org

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-mode-groups)

(use-package org
  :ensure t
  :defer  t
  :idle   (require 'org)
  :init
  (progn

    (after 'evil
      (evil-define-key 'normal org-mode-map (kbd "M-P") 'outline-previous-visible-heading)
      (evil-define-key 'normal org-mode-map (kbd "M-N") 'outline-next-visible-heading))

    (hook-fn 'cb:org-minor-modes-hook
      "Diminish org minor modes."
      (--each cb:org-minor-modes
        (ignore-errors (diminish it))))

    (setq
     org-directory (concat user-home-directory "org/")
     org-default-notes-file (concat org-directory "notes.org"))

    (declare-modal-executor org-notes
      :command (find-file org-default-notes-file)
      :bind    "M-O")

    (when (or (daemonp) (display-graphic-p))
      (setq initial-buffer-choice org-default-notes-file)))

  :config
  (progn

    (defmacro with-previous-buffer (&rest forms)
      `(with-current-buffer (nth 1 (buffer-list))
         ,@body))

    (defmacro prev-str-val (sym)
      `(or (ignore-errors
             (with-previous-buffer
              ,sym))
           ""))

    (setq
     org-catch-invisible-edits 'smart
     org-pretty-entities       t

     org-capture-templates
     '(("t" "Todo" entry
        (file+headline org-default-notes-file "Tasks")
        "* TODO %?\n %i\n")

       ("r" "Reading List" entry
        (file+headline org-default-notes-file "Reading List")
        "* %?\n %i\n")

       ("l" "Link" entry
        (file+headline org-default-notes-file "Links")
        "* %(prev-str-val w3m-buffer-title)%?\n %(prev-str-val w3m-current-url)\n%i\n")

       ("n" "Note" item
        (file+headline org-default-notes-file "Notes")
        "- %?\n %i\n"))

     org-todo-keywords
     '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

    (--each '("NOTES" "COMMENTS")
      (add-to-list 'org-drawers it))

    (hook-fn 'org-mode-hook
      (auto-revert-mode +1)
      (unless (buffer-file-name)
        (cb:append-buffer))

      ;; HACK: Something in org's setup interferes with input method. Wait
      ;; til after buffer is fully initialized before setting.
      (run-with-timer 0.02 nil 'set-input-method "TeX"))

    (define-key org-mode-map (kbd "M-p") 'org-metaup)
    (define-key org-mode-map (kbd "M-n") 'org-metadown)))

(use-package org-capture
  :commands (org-capture)
  :init
  (progn
    (when cb:use-vim-keybindings?
      (bind-key "M-o" 'org-capture))

    (hook-fn 'org-capture-mode-hook
     (when (fboundp 'evil-insert)
       (evil-insert 0))))
  :config
  (progn

    (defun cb:sort-tasks-in-subtree ()
      "Sort child elements of the tree at point."
      (let ((beg (1+ (line-number-at-pos)))
            (end (save-excursion
                   (org-mark-subtree)
                   (region-end))))
        (push-mark beg)
        (push-mark end)
        (org-sort-entries t 112)))

    (defun cb:sort-todos-by-priority ()
      "Sort the Tasks list in the notes file."
      (ignore-errors
        (with-current-buffer (find-file-noselect org-default-notes-file)
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp (rx bol "*" (+ space) "Tasks" (* space) eol) nil t)
            (cb:sort-tasks-in-subtree)))))

    (add-hook 'org-capture-after-finalize-hook 'cb:sort-todos-by-priority)))

(provide 'cb-org)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-org.el ends here
