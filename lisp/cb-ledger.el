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
(autoload 'ido-completing-read "ido")

(defvar ledger-file (f-join org-directory "accounts.ledger"))
(defvar ledger-transaction-inserted-hook nil
  "Hook run after a transaction has been inserted.")
(defvar ledger-expense-inserted-hook nil
  "Hook run after an expense transaction has been inserted.")

(add-to-list 'org-action-picker-options
             '("$" "Go to ledger" (lambda () (find-file ledger-file))))

(after 'org-capture

  (defun ledger-capture-expense ()
    (with-current-buffer (find-file-noselect ledger-file)
      (call-interactively 'ledger-add-expense)))

  (add-to-list 'org-capture-templates
               `("$" "Expense" plain (file ,ledger-file)
                 (function ledger-capture-expense)
                 :clock-keep t
                 :immediate-finish t)))

(add-hook 'ledger-mode-hook 'linum-mode)

;; `ledger-mode' provides support for ledger files.
(use-package ledger-mode
  :ensure t
  :commands (ledger-mode)
  :mode ("\\.ledger$" . ledger-mode)
  :config
  (progn
    (setq ledger-post-account-alignment-column 2)

    ;; Custom reports.

    (--each
        '(("net worth" "ledger -f %(ledger-file) bal ^assets ^liabilities")
          ("cash flow" "ledger -f %(ledger-file) bal ^income ^expenses")
          ("week budget"
           "ledger -f %(ledger-file) -p 'this month' --add-budget --weekly reg checking"))
      (add-to-list 'ledger-reports it t))

    ;; Custom commands.

    (defun ledger-move-to-date (date)
      "Move to DATE in the ledger file."
      ;; Use slashes for consistency with ledger's date format.
      (interactive (list (s-replace "-" "/" (org-read-date))))
      (cl-destructuring-bind (y m d) (-map 'string-to-number (s-split "/" date))
        (ledger-xact-find-slot (encode-time 0 0 0 d m y))))

    (defun ledger-insert-transaction-header (date payee reference &optional arg)
      "Insert a header at point. DATE, PAYEE and REFERENCE are all strings.
With prefix ARG, insert at point. Otherwise move to an appropriate buffer pos."
      (interactive (list
                    (org-read-date nil nil nil "Transaction")
                    (ido-completing-read "Payee: " (ledger-payees-in-buffer))
                    (s-trim (read-string "[Reference]: "))
                    current-prefix-arg))
      ;; Only insert header if there is no identical header at point.
      (let ((dt (s-replace "-" "/" date))
            (ref (if (s-blank? reference) " " (format " (%s) " reference))))
        (unless arg
          (ledger-move-to-date dt))
        (unless (and (s-matches? (regexp-quote dt) (current-line -1))
                     (s-matches? (regexp-quote payee) (current-line -1))
                     (s-matches? (regexp-quote ref) (current-line -1)))
          (newline)
          (goto-char (line-beginning-position))
          (insert (concat dt ref payee))
          (newline)
          (indent-to ledger-post-account-alignment-column)))
      (run-hooks 'ledger-transaction-inserted-hook))

    (defun cbledger:accounts ()
      "Find all accounts in the current buffer."
      (->> (s-match-strings-all
            (rx bol (+ space) (group (+ (not space)) ":" (+? nonl))
                (or eol "  "))
            (buffer-string-no-properties))
        (-map 'cadr)
        (-uniq)
        (-sort 'string<)))

    (defun ledger-add-expense ()
      "Insert an expense transaction at the appropriate place for the given date.
With prefix ARG, insert at point."
      (interactive "*")
      (atomic-change-group
        (call-interactively 'ledger-insert-transaction-header)

        (let ((amount (read-number "Amount $: "))
              (account
               (ido-completing-read "To Account: " (cbledger:accounts)
                                    nil nil "Expenses:"))
              (balancing-account
               (ido-completing-read "From Account: " (cbledger:accounts)
                                    nil nil "Assets:")))

          (when (or (s-matches? "checking" balancing-account)
                    (y-or-n-p "Transaction cleared? "))
            (ledger-toggle-current-transaction))

          (insert account)
          (insert (format "  $ %.2f" amount))
          (newline)
          (indent-to ledger-post-account-alignment-column)
          (insert balancing-account)
          (open-line 2)
          (ledger-format-buffer)))

      (run-hooks 'ledger-expense-inserted-hook))

    (defun ledger-format-buffer ()
      "Reformat the buffer."
      (interactive "*")
      (ledger-post-align-postings (point-min) (point-max))
      (ledger-sort-buffer)
      (message "Formatted buffer"))

    (hook-fn 'ledger-mode-hook
      (add-hook 'before-save-hook 'ledger-format-buffer nil t))

    (defun ledger-ret ()
      "Newline and format."
      (interactive "*")
      (cond
       ((s-matches? (rx bol (* space) eol) (current-line))
        (delete-horizontal-space)
        (newline))
       (t
        (ledger-post-align-postings)
        (newline)
        (indent-to ledger-post-account-alignment-column))))

    (defun ledger-prev-transaction (&optional count)
      "Move backward to the start of the last transaction."
      (interactive)
      (goto-char (line-beginning-position))
      (cond
       ((bobp)
        (user-error "Beginning of buffer"))
       ((search-backward-regexp (rx bol digit) nil t count)
        (goto-char (line-beginning-position)))
       (t
        (goto-char (point-min)))))

    (defun ledger-next-transaction (&optional count)
      "Move forward to the start of the next transaction."
      (interactive)
      (let ((start (point)))
        (goto-char (line-end-position))
        (if (search-forward-regexp (rx bol digit) nil t count)
            (goto-char (line-beginning-position))
          (goto-char start)
          (user-error "End of buffer"))))

    (cl-defun ledger-transaction-at-pt (&optional (pt (point)))
      "Return the transaction at PT."
      (goto-char pt)
      (-when-let* ((beg (ledger-transaction-start-pos))
                   (end (ledger-transaction-end-pos)))
        (buffer-substring-no-properties beg end)))

    (cl-defun ledger-cur-transaction-date ()
      "Return the date for the transaction at point"
      (-when-let (trans (ledger-transaction-at-pt))
        (car (s-match ledger-full-date-regexp trans))))

    (defun ledger-transaction-start-pos ()
      "Return the buffer position where the transaction at point begins."
      (save-excursion
        (if (s-matches? (rx bol (any digit)) (current-line))
            (goto-char (line-beginning-position))
          (ignore-errors
            (ledger-prev-transaction)))))

    (defun ledger-transaction-end-pos ()
      "Return the buffer position where the transaction at point ends."
      (save-excursion
        (or (ignore-errors (ledger-next-transaction))
            (goto-char (point-max)))

        (let (pos)
          (forward-line -1)
          (goto-char (line-end-position))
          (setq pos (point))
          (while (s-matches? (rx bol (* space) eol) (current-line))
            (forward-line -1)
            (goto-char (line-end-position))
            (setq pos (point)))
          pos)))

    (defun cbledger:delete-transaction-at-pt ()
      "Kill the current transaction.
Behaves correctly for transactions that are not separated by blank lines."
      (when (s-blank? (ledger-transaction-at-pt))
        (error "Not at a transaction"))
      (delete-region (ledger-transaction-start-pos)
                     (ledger-transaction-end-pos))
      (collapse-vertical-whitespace)
      (forward-line))

    (defun ledger-kill-transaction-at-pt ()
      "Kill the transaction at point and add it to the kill ring."
      (interactive "*")
      (-if-let (trans (ledger-transaction-at-pt))
          (progn
            (kill-new trans)
            (cbledger:delete-transaction-at-pt)
            (when (called-interactively-p)
              (message "Transaction copied to kill-ring"))
            trans)
        (user-error "Point is not at a transaction")))

    (defun ledger-transpose-transactions ()
      "Swap the current transaction with the preceding one.
The transactions must have matching dates."
      (interactive "*")

      (unless (s-matches? (rx bol (any digit)) (current-line))
        (ledger-prev-transaction))

      (let ((trans (ledger-transaction-at-pt))
            (date (ledger-cur-transaction-date)))
        (cond
         ((null trans)
          (user-error "Point is not at a valid transaction"))
         ((null date)
          (error "Invalid date for current transaction"))

         ((save-excursion
            (ledger-prev-transaction)
            (equal date (ledger-cur-transaction-date)))

          (unwind-protect
              (atomic-change-group
                (ledger-kill-transaction-at-pt)
                (ledger-prev-transaction)
                (forward-line -1)
                (unless (bobp) (newline))
                (save-excursion
                  (insert trans)
                  (collapse-vertical-whitespace)))

            (pop kill-ring)))

         (t
          (user-error "Transaction dates do not match")))))

    (defun ledger-move-transaction-up ()
      "Move the current transaction up.
Signal an error of doing so would break date ordering."
      (interactive "*")
      (ledger-transpose-transactions))

    (defun ledger-move-transaction-down ()
      "Move the current transaction down.
Signal an error of doing so would break date ordering."
      (interactive "*")
      (let ((pt (point)))
        (unwind-protect
            (progn
              (ledger-next-transaction)
              (ledger-transpose-transactions))
          (goto-char pt))
        (ledger-next-transaction)))

    (defun cbledger:set-key-bindings ()
      (local-set-key (kbd "RET")     'ledger-ret)
      (local-set-key (kbd "M-<up>") 'ledger-move-transaction-up)
      (local-set-key (kbd "M-<down>") 'ledger-move-transaction-down)
      (local-set-key (kbd "M-P") 'ledger-prev-transaction)
      (local-set-key (kbd "M-N") 'ledger-next-transaction)
      (local-set-key (kbd "C-c C-k") 'ledger-kill-transaction-at-pt)
      (local-set-key (kbd "C-c C-c") 'ledger-report)
      (local-set-key (kbd "C-c C-t") 'ledger-insert-transaction-header)
      (local-set-key (kbd "C-c C-e") 'ledger-add-expense)
      (local-set-key (kbd "C-c C-d") 'ledger-move-to-date)
      (local-set-key (kbd "M-RET")   'ledger-toggle-current-transaction)
      (local-set-key (kbd "M-q")     'ledger-format-buffer))

    (add-hook 'ledger-mode-hook 'cbledger:set-key-bindings)

    (after 'ledger-mode
      ;; FIX: Modify function to prevent errors passing nil string to
      ;; regexp-quote.
      (defun ledger-report-payee-format-specifier ()
        (let ((payee (ledger-xact-payee)))
          (ledger-read-string-with-default
           "Payee" (when payee (regexp-quote payee))))))))

;; Customise font locking.
(after 'ledger-mode
  (set-face-background 'ledger-font-xact-highlight-face nil)
  (set-face-foreground 'ledger-font-comment-face
                       (face-foreground 'font-lock-comment-face))
  (set-face-foreground 'ledger-font-posting-account-face
                       (face-foreground 'default))

  (set-face-foreground 'ledger-font-pending-face solarized-hl-orange)
  (set-face-foreground 'ledger-font-payee-cleared-face solarized-hl-green)
  (set-face-foreground 'ledger-font-payee-uncleared-face solarized-hl-orange)

  (defface ledger-date
    `((t :inherit org-date :underline nil :foreground ,solarized-hl-cyan))
    "Face for dates at start of transactions."
    :group 'ledger-faces)

  (defface ledger-periodic-header
    `((t :foreground ,solarized-hl-violet :bold t))
    "Face for the header for periodic transactions."
    :group 'ledger-faces)

  (defface ledger-year-line
    `((t :foreground ,solarized-hl-violet))
    "Face for year declarations."
    :group 'ledger-faces)

  (font-lock-add-keywords
   'ledger-mode
   `((,(rx bol (+ (any digit "/"))) . 'ledger-date)
     (,(rx bol "~" (* nonl)) . 'ledger-periodic-header)
     (,(rx bol "year" (+ space) (+ digit) (* space) eol) . 'ledger-year-line))))

;; Configure hideshow.
(after 'hideshow

  (defun cbledger:hs-forward (&rest _)
    (forward-line 1)
    (or (when (search-forward-regexp (rx bol digit) nil t)
          (forward-line -1)
          (goto-char (line-end-position))
          t)
        (goto-char (point-max))))

  (add-hook 'ledger-mode-hook 'hs-hide-all)

  (add-to-list 'hs-special-modes-alist
               `(ledger-mode
                 ,(rx bol digit)
                 nil
                 nil
                 cbledger:hs-forward
                 nil)))

(after 'evil

  (defun cbledger:set-report-keys ()
    "Set key bindings for ledger-report-mode."
    (evil-local-set-key 'normal (kbd "e") 'ledger-report-edit)
    (evil-local-set-key 'normal (kbd "q") 'ledger-report-kill)
    (evil-local-set-key 'normal (kbd "r") 'ledger-report-redo)
    (evil-local-set-key 'normal (kbd "s") 'ledger-report-save))

  (add-hook 'ledger-report-mode-hook 'cbledger:set-report-keys)
  (add-hook 'ledger-transaction-inserted-hook 'evil-insert-state))

(provide 'cb-ledger)

;;; cb-ledger.el ends here
