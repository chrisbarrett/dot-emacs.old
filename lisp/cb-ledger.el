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

;; `ledger-mode' provides support for ledger files.
(use-package ledger-mode
  :ensure t
  :commands (ledger-mode)
  :mode ("\\.ledger$" . ledger-mode)
  :config
  (progn
    (setq ledger-post-account-alignment-column 2)

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

    (defun cbledger:add-expense
      (date payee amount account balancing-account reference
            &optional insert-at-point)
      "Insert an expense transaction at the appropriate place for the given date.

* DATE is a string of the form YYYY/MM/DD

* Unless prefix arg INSERT-AT-POINT is set, find the correct
  place to perform the insertion."
      (interactive (list
                    (org-read-date nil nil nil "Transaction")
                    (read-string "Payee: ")
                    (read-number "Amount $: ")
                    (read-string "Account: " "Expenses:")
                    (read-string "From Account: " "Assets:Checking")
                    (let ((ref (s-trim (read-string "[Reference]: "))))
                      (if (s-blank? ref)
                          (format "(%s)" ref)))

                    current-prefix-arg))

      (unless insert-at-point
        (cbledger:move-to-date date))

      ;; Only insert header if there is no identical header at point.
      (let ((header (concat (s-replace "-" "/" date)
                            (if reference (concat reference " ") "")
                            payee
                            "\n  " account)))
        (unless (s-matches? (regexp-quote header) (current-line))
          (insert header)))

      (indent-to ledger-post-account-alignment-column)
      (insert (format "$ %s" amount))
      (insert (concat "\n  " balancing-account))
      (open-line 2)
      (message "Inserted new transaction"))

    (bind-keys
      :map ledger-mode-map
      "C-c C-t" 'ledger-toggle-current
      "C-c C-e" 'cbledger:add-expense
      "C-c C-d" 'cbledger:move-to-date)

    (after 'ledger-mode
      ;; FIX: Modify function to prevent errors passing nil string to
      ;; regexp-quote.
      (defun ledger-report-payee-format-specifier ()
        (let ((payee (ledger-xact-payee)))
          (ledger-read-string-with-default
           "Payee" (when payee (regexp-quote payee))))))))

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
