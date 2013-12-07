;;; cb-ledger.el --- Supporting configuration for the Ledger accounting tool.

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

;; Supporting configuration for the Ledger accounting tool.

;;; Code:

(require 'cb-lib)
(require 'use-package)
(require 'cb-org)
(autoload 'org-read-date "org")

(defvar ledger-file (f-join org-directory "accounts.ledger"))

(add-to-list 'org-action-picker-options
             '("$" "Go to ledger" (lambda () (find-file ledger-file))))

(after 'org-capture

  (defun ledger-capture-expense ()
    (with-current-buffer (find-file-noselect ledger-file)
      (call-interactively 'cbledger:add-expense)))

  (add-to-list 'org-capture-templates
               `("$" "Expense" plain (file ,ledger-file)
                 (function ledger-capture-expense)
                 :clock-keep t
                 :immediate-finish t)))

;; `ledger-mode' provides support for ledger files.
(use-package ledger-mode
  :ensure t
  :commands (ledger-mode)
  :mode ("\\.ledger$" . ledger-mode)
  :config
  (progn
    (setq ledger-post-account-alignment-column 2)

    (set-face-background 'ledger-font-xact-highlight-face nil)

    ;; Custom reports.

    (--each '(("net worth" "ledger -f %(ledger-file) bal ^assets ^liabilities")
              ("cash flow" "ledger -f %(ledger-file) bal ^income ^expenses"))
      (add-to-list 'ledger-reports it t))

    ;; Custom commands.

    (defun cbledger:move-to-date (date)
      "Move to DATE in the ledger file."
      ;; Use slashes for consistency with ledger's date format.
      (interactive (list (s-replace "-" "/" (org-read-date))))
      (cl-destructuring-bind (y m d) (-map 'string-to-number (s-split "/" date))
        (ledger-xact-find-slot (encode-time 0 0 0 d m y))))

    (defun cbledger:insert-header (date payee reference &optional arg)
      "Insert a header at point. DATE, PAYEE and REFERENCE are all strings.
With prefix ARG, insert at point. Otherwise move to an appropriate buffer pos."
      (interactive (list
                    (org-read-date nil nil nil "Transaction")
                    (read-string "Payee: ")
                    (s-trim (read-string "[Reference]: "))
                    current-prefix-arg))
      ;; Only insert header if there is no identical header at point.
      (let ((dt (s-replace "-" "/" date))
            (ref (if (s-blank? reference) " " (format " (%s) " reference))))
        (unless arg
          (cbledger:move-to-date dt))
        (unless (and (s-matches? (regexp-quote dt) (current-line -1))
                     (s-matches? (regexp-quote payee) (current-line -1))
                     (s-matches? (regexp-quote ref) (current-line -1)))
          (goto-char (line-beginning-position))
          (insert (concat dt ref payee))
          (newline)
          (indent-to ledger-post-account-alignment-column))))

    (defun cbledger:add-expense ()
      "Insert an expense transaction at the appropriate place for the given date.
With prefix ARG, insert at point."
      (interactive)
      (call-interactively 'cbledger:insert-header)

      (let ((amount (read-number "Amount $: "))
            (account (read-string "Account: " "Expenses:"))
            (balancing-account (read-string "From Account: " "Assets:Checking")))

        (insert account)
        (insert (format "  $ %.2f" amount))
        (ledger-post-align-xact (point))
        (newline)
        (indent-to ledger-post-account-alignment-column)
        (insert balancing-account)
        (open-line 2)))

    (defun cbledger:format-buffer ()
      "Reformat the buffer."
      (interactive "*")
      (ledger-post-align-postings (point-min) (point-max)))

    (bind-keys
      :map ledger-mode-map
      "C-c C-c" 'ledger-report
      "C-c C-h" 'cbledger:insert-header
      "C-c C-t" 'ledger-toggle-current
      "C-c C-e" 'cbledger:add-expense
      "C-c C-d" 'cbledger:move-to-date
      "M-q"     'cbledger:format-buffer)

    (after 'ledger-mode
      ;; FIX: Modify function to prevent errors passing nil string to
      ;; regexp-quote.
      (defun ledger-report-payee-format-specifier ()
        (let ((payee (ledger-xact-payee)))
          (ledger-read-string-with-default
           "Payee" (when payee (regexp-quote payee))))))))

;; Configure hideshow.
(after 'hideshow

  (defun cbledger:hs-forward (&optional n)
    (forward-line 1)
    (or (when (search-forward-regexp (rx bol digit) nil t)
          (forward-line -1)
          (goto-char (line-end-position))
          t)

        (goto-char (point-max))))

  (add-to-list 'hs-special-modes-alist
               `(ledger-mode
                 ,(rx bol digit)
                 nil
                 nil
                 cbledger:hs-forward
                 nil)))

(after 'evil

  (after 'ledger-mode
    (defadvice ledger-add-transaction (after insert-state activate)
      "Enter evil insert state after adding transaction."
      (when (fboundp 'evil-insert-state)
        (evil-insert-state))))

  (defun cbledger:set-report-keys ()
    "Set key bindings for ledger-report-mode."
    (evil-local-set-key 'normal (kbd "e") 'ledger-report-edit)
    (evil-local-set-key 'normal (kbd "q") 'ledger-report-kill)
    (evil-local-set-key 'normal (kbd "r") 'ledger-report-redo)
    (evil-local-set-key 'normal (kbd "s") 'ledger-report-save))

  (add-hook 'ledger-report-mode-hook 'cbledger:set-report-keys))

(provide 'cb-ledger)

;;; cb-ledger.el ends here
