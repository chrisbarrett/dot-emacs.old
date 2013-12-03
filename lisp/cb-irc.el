;;; cb-irc.el --- Configuration for IRC clients.

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

;; Configuration for IRC clients.

;;; Code:

(require 'cb-lib)
(require 'use-package)
(require 'cb-colour)

;; `circe' is an Emacs IRC client.
(use-package circe
  :commands circe
  :ensure t
  :config
  (progn

    (require 'lui-autopaste)
    (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
    (setq circe-reduce-lurker-spam t)

    (after 'lui

      (setq lui-flyspell-p t)

      ;; Show timestamps in margin

      (setq lui-time-stamp-position 'right-margin
            lui-time-stamp-format "%H:%M")

      (defun cbcirce:set-margin ()
        (setq right-margin-width 5))

      (add-hook 'lui-mode-hook 'cbcirce:set-margin)

      ;; Resize window gracefully

      (setq lui-fill-type nil)

      (defun cbcirce:wrapping-setup ()
        (setq
         fringes-outside-margins t
         right-margin-width 5
         word-wrap t
         wrap-prefix "    "))

      (add-hook 'lui-mode-hook 'cbcirce:wrapping-setup))

    ;; Customise faces

    (set-face-foreground 'circe-server-face (face-foreground 'font-lock-comment-face))
    (set-face-foreground 'circe-my-message-face solarized-hl-orange)
    (set-face-foreground 'lui-button-face solarized-hl-yellow)
    (set-face-foreground 'circe-originator-face solarized-hl-violet)
    (set-face-foreground 'circe-highlight-nick-face solarized-hl-cyan)
    (set-face-foreground 'lui-time-stamp-face (face-foreground 'font-lock-comment-face))

    ;; Customise prompt

    (set-face-foreground 'circe-prompt-face solarized-hl-orange)
    (set-face-background 'circe-prompt-face nil)

    (defun cbcirce:set-prompt ()
      (let ((prompt (propertize (format "%s: " (circe-server-nick)) 'face circe-prompt-face)))
        (lui-set-prompt prompt)))

    (add-hook 'circe-nickserv-authenticated-hook 'cbcirce:set-prompt)
    (add-hook 'circe-server-connected-hook 'cbcirce:set-prompt)
    (add-hook 'circe-channel-mode-hook 'cbcirce:set-prompt)))

;; Use evil insert state in circe
(after 'evil
  (--each '(circe-server-mode-hook circe-channel-mode-hook circe-chat-mode-hook)
    (add-hook it 'evil-insert-state)))

;; Fix delete key behaviour in circe message mode.
(after '(circe smartparens)

  (defun cbcirce:del ()
    "Delete command for Circe buffers that works with smartparens."
    (interactive)
    (call-interactively
     (if (sp-get-sexp t) 'sp-backward-delete-char 'delete-backward-char)))

  (define-key circe-channel-mode-map (kbd "<backspace>") 'cbcirce:del))

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

(provide 'cb-irc)

;;; cb-irc.el ends here
