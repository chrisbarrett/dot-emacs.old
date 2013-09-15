;;; cb-wanderlust.el --- Configuration for wanderlust email client.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130915.0549

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

;; Configuration for wanderlust email client.

;;; Code:

(require 'cb-lib)

;; `wanderlust' email client.
(use-package wl
  :commands (wl wl-other-frame)
  :init
  (progn
    (defvar wl-installation (f-join cb:el-get-dir "wanderlust"))
    (defvar wl-icon-directory (f-join wl-installation "icons"))

    (--each (list (f-join wl-installation "wl")
                  (f-join wl-installation "utils")
                  (f-join wl-installation "elmo")
                  (f-join cb:el-get-dir "flim")
                  (f-join cb:el-get-dir "semi")
                  )
      (add-to-list 'load-path it)))
  :config
  (progn

    (define-mode-group cb:wl-modes
      '(wl-draft-editor-mode
        wl-original-message-mode
        wl-news-mode
        wl-summary-mode
        wl-template-mode
        wl-message-mode
        wl-folder-mode
        wl-score-mode
        wl-summary-buffer-display-header-mode
        wl-highlight-background-mode
        wl-draft-mode
        wl-plugged-mode
        wl-message-decode-mode
        wl-summary-buffer-display-mime-mode))

    ;; Set custom compose functions.
    (hook-fn 'cb:wl-modes-hook
      (local-set-key (kbd "w") 'cb-org:compose-mail))

    ;; Disable smartparens
    (hook-fn 'cb:wl-modes-hook
      (when (true? smartparens-mode)
        (smartparens-mode -1)))

    (hook-fn 'wl-folder-mode-hook
      (local-set-key (kbd "j") 'wl-folder-next-entity)
      (local-set-key (kbd "k") 'wl-folder-prev-entity))

    (hook-fn 'wl-summary-mode-hook
      (local-set-key (kbd "j") 'wl-summary-next)
      (local-set-key (kbd "k") 'wl-summary-prev))

    (defun configure-wanderlust ()
      "Configure wanderlust for your email setup."
      (interactive)
      (el-get-install 'wanderlust)
      (url-retrieve
       "https://raw.github.com/gist/1207807/wl-auto-config.el"
       (lambda (_)
         (goto-char (point-max))
         (eval-print-last-sexp))))

    ;; Much of this config is taken from
    ;;   http://emacs-fu.blogspot.co.nz/2009/06/e-mail-with-wanderlust.html

    (setq
     ;; Configure elmo to find ~/Maildir
     elmo-maildir-folder-path (f-join user-home-directory "Maildir")
     wl-fcc ".sent"                       ;; sent msgs go to the "sent"-folder
     wl-fcc-force-as-read t               ;; mark sent messages as read
     wl-default-folder ".inbox"
     wl-draft-folder ".drafts"
     wl-trash-folder ".archive"
     wl-spam-folder ".trash"
     ;; Hide fields from message buffers
     wl-message-ignored-field-list '("^.*:")
     wl-message-visible-field-list
     '("^\\(To\\|Cc\\):"
       "^Subject:"
       "^\\(From\\|Reply-To\\):"
       "^Organization:"
       "^Message-Id:"
       "^\\(Posted\\|Date\\):")

     wl-message-sort-field-list
     '("^From"
       "^Organization:"
       "^X-Attribution:"
       "^Subject"
       "^Date"
       "^To"
       "^Cc")

     wl-summary-showto-folder-regexp (rx (* nonl) (or "Sent" "sent") (* nonl))
     wl-summary-move-direction-toggle nil
     wl-summary-line-format "%n%T%P %D/%M (%W) %h:%m %t%[%25(%c %f%) %] %s"
     wl-summary-width 150
     wl-forward-subject-prefix "Fwd: "
     ;; Make reply-to-sender the default, not reply-to-all.
     wl-draft-reply-without-argument-list
     '(("Reply-To" ("Reply-To") nil nil)
       ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
       ("From" ("From") nil nil))
     wl-draft-reply-with-argument-list
     '(("Followup-To" nil nil ("Followup-To"))
       ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
       ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
       ("From" ("From") ("To" "Cc") ("Newsgroups")))
     ;; mutt-style buffer behaviour
     wl-summary-always-sticky-folder-list t)

    (add-to-list 'wl-summary-sort-specs 'rdate)

    ;; Validate mails when sending.

    (defun cb-wl:check-subject ()
      "Prompt to continue sending a message with no subject."
      (when (s-blank? (s-trim (std11-field-body "Subject")))
        (unless (y-or-n-p "Subject is blank.  Really send? ")
          (error "Cancelled"))))

    (add-hook 'wl-mail-send-pre-hook 'cb-wl:check-subject)

    (after 'evil
      ;; Disable evil for wanderlust modes.
      (hook-fn 'wl-folder-mode-hook
        (evil-emacs-state))
      (--each cb:wl-modes
        (add-to-list 'evil-emacs-state-modes it)))))

(declare-modal-executor wanderlust
  :restore-bindings '("q")
  :bind "M-M"
  :command wl)

;; `mime-view' provides MIME display functions.
(use-package mime-view
  :defer t
  :config
  (progn
    ;; Advise modes to run hooks.
    (defadvice mime-view-mode (after run-hook activate)
      (run-hooks 'mime-view-mode-hook))
    ;; <q> closes messages.
    (after 'evil
      (hook-fn 'mime-view-mode-hook
        (evil-local-set-key 'normal "q" 'delete-window )))))


(provide 'cb-wanderlust)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-wanderlust.el ends here
