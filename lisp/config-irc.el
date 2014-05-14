;;; config-irc.el --- IRC configuration

;; Copyright (C) 2014 Chris Barrett

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

;; IRC configuration

;;; Code:

(require 'utils-common)
(require 'utils-ui)

(cb:install-package 'circe)

(setq lui-flyspell-p t)

(after 'circe (require 'lui-autopaste))
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(setq lui-time-stamp-position 'right-margin
      lui-time-stamp-format "%H:%M")

(put 'lui-mode 'right-margin-width 5)

(defun show-irc ()
  "Show all IRC buffers."
  (interactive)
  (with-window-restore
    ;; Start IRC.
    (unless (--first-buffer (derived-mode-p 'circe-server-mode))
      (call-interactively 'circe))
    ;; Select IRC buffers.
    (let ((bufs (->> (--filter-buffers (derived-mode-p 'circe-chat-mode
                                                       'circe-channel-mode))
                  (-sort (-on 'string< 'buffer-name)))))
      ;; Show all IRC buffers.
      (expose-buffers bufs)
      ;; Set up restore bindings.
      (--each bufs (buffer-local-set-key (kbd "C-c C-k") (command (restore)))))))

(setq lui-fill-type nil)

(defun cbcirce:wrapping-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "    "))

(add-hook 'lui-mode-hook 'cbcirce:wrapping-setup)

(setq circe-bot-list '("fsbot" "rudybot"))

(setq circe-reduce-lurker-spam t)

(defun cbcirce:message-option-bot (nick &rest ignored)
  (when (member nick circe-bot-list)
    '((text-properties . (face circe-fool-face lui-do-not-track t)))))

(add-hook 'circe-message-option-functions 'cbcirce:message-option-bot)

(after 'circe
  (set-face-foreground 'circe-server-face
                       (face-foreground 'font-lock-comment-face))

  (set-face-foreground 'circe-fool-face
                       (face-foreground 'font-lock-comment-face))

  (set-face-foreground 'lui-time-stamp-face
                       (face-foreground 'font-lock-comment-face))

  (set-face-foreground 'circe-my-message-face solarized-hl-orange)
  (set-face-foreground 'lui-button-face solarized-hl-yellow)
  (set-face-foreground 'circe-originator-face solarized-hl-violet)
  (set-face-foreground 'circe-highlight-nick-face solarized-hl-cyan))

(defun cbcirce:set-prompt ()
  (let ((prompt (propertize (format "%s: " (circe-server-nick))
                            'face circe-prompt-face)))
    (lui-set-prompt prompt)))

(add-hook 'circe-nickserv-authenticated-hook 'cbcirce:set-prompt)
(add-hook 'circe-server-connected-hook 'cbcirce:set-prompt)
(add-hook 'circe-channel-mode-hook 'cbcirce:set-prompt)

(after 'circe
  (set-face-foreground 'circe-prompt-face solarized-hl-orange)
  (set-face-background 'circe-prompt-face nil))

(after '(evil circe)
  (add-hook 'circe-server-mode-hook 'evil-insert-state)
  (add-hook 'circe-channel-mode-hook 'evil-insert-state)
  (add-hook 'circe-chat-mode-hook 'evil-insert-state))

(defun cbcirce:del ()
  "Delete command for Circe buffers that works with smartparens."
  (interactive)
  (call-interactively
   (if (sp-get-sexp t) 'sp-backward-delete-char 'delete-backward-char)))

(after '(circe smartparens)
  (define-key circe-channel-mode-map (kbd "<backspace>") 'cbcirce:del))

(provide 'config-irc)

;;; config-irc.el ends here
