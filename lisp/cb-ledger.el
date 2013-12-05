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

    (after 'ledger-mode
      ;; FIX: Modify function to prevent errors passing nil string to
      ;; regexp-quote.
      (defun ledger-report-payee-format-specifier ()
        (let ((payee (ledger-xact-payee)))
          (ledger-read-string-with-default "Payee" (when payee (regexp-quote payee))))))))

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
