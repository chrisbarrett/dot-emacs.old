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

;; `erc' is an Emacs IRC client.
(use-package erc
  :commands erc
  :config
  (progn

    (after 'erc
      (setq
       erc-hide-list
       '("JOIN" "PART" "QUIT" "NICK")

       erc-prompt
       (lambda () (format "%s>" (erc-current-nick)))

       erc-track-exclude-types
       '("JOIN" "NICK" "PART" "QUIT" "MODE"
         "324" "329" "332" "333" "353" "477")))

    (erc-autojoin-mode +1)
    (erc-track-mode +1)))

;; `circe' is a better Emacs IRC client.
(use-package circe
  :commands circe
  :ensure t
  :config
  (progn
    ;; Use evil insert state.
    (after 'evil
      (--each '(circe-server-mode-hook circe-channel-mode-hook circe-chat-mode-hook)
        (add-hook it 'evil-insert-state)))

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

    (set-face-foreground circe-server-face (face-foreground 'font-lock-comment-face))
    (set-face-foreground 'lui-button-face solarized-hl-yellow)
    (set-face-foreground circe-originator-face solarized-hl-green)
    (set-face-foreground circe-prompt-face solarized-hl-red)
    (set-face-background circe-prompt-face nil)
    ))

(provide 'cb-irc)

;;; cb-irc.el ends here
