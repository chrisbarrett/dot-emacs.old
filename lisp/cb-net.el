;;; cb-net.el --- Configuration for web and Internet stuff

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0018

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

;; Configuration for web and Internet stuff

;;; Code:

(require 'cb-foundation)
(require 'use-package)
(require 's)
(require 'async)
(require 'cb-typefaces)
(autoload 'mail-add-attachment "sendmail")
(autoload 'thing-at-point-url-at-point "thingatpt")
(autoload 'message-field-value "message")
(autoload 'smtpmail-send-it "smtpmail")

;; Use the `async' library to send mail messages asynchronously, ie. without
;; blocking emacs!

(defvar async-smtpmail-sent-hook nil)

(defun async-smtpmail-send-it (&rest _)
  "Send mail asynchronously using another Emacs process."
  (let ((to (message-field-value "To")))
    (message "Delivering message to %s..." to)
    (async-start
     `(lambda ()
        (require 'smtpmail)
        (with-temp-buffer
          (insert ,(buffer-substring-no-properties (point-min) (point-max)))
          ;; Pass in the variable environment for smtpmail
          ,(async-inject-variables "\\`\\(smtpmail\\|\\(user-\\)?mail\\)-")
          (smtpmail-send-it)))
     `(lambda (&optional ignore)
        (message "Delivering message to %s...done" ,to)
        (run-hooks 'async-smtpmail-sent-hook)))))

(setq message-send-mail-function 'async-smtpmail-send-it
      send-mail-function 'async-smtpmail-send-it)

;; `message' provides emailing and messaging functions.
(use-package message
  :config
  (progn

    (defun mail-add-attachment-ido (file)
      (interactive (list (ido-read-file-name "Attach file: " )))
      (mail-add-attachment file))

    (define-key message-mode-map (kbd "C-c C-a") 'mail-add-attachment-ido)))

;; `bbdb' provides an address book for emacs and integrates with many modes.
(use-package bbdb
  :ensure t
  :defer  t
  :init
  (progn
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
    (add-hook 'message-mode-hook 'bbdb-insinuate-mail)
    (defvar bbdb-file (concat user-dropbox-directory ".bbdb")))
  :config
  (progn
    ;; Wrap config in `after' form to prevent warnings.
    (after 'bbdb
      (setq
       bbdb-offer-save 1
       bbdb-use-popup t
       bbdb-electric t
       bbdb-pop-up-window-size 0.5
       bbdb-mua-popup-window-size 4
       bddb-popup-target-lines 1
       bbdb-dwim-net-address-allow-redundancy t
       bbdb-quiet-about-name-mismatches 2
       bbdb-always-add-address t
       bbdb-canonicalize-redundant-nets-p t
       bbdb-completion-type nil
       bbdb-complete-name-allow-cycling t
       bbbd-message-caching-enabled t
       bbdb-use-alternate-names t
       bbdb-elided-display t
       bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
       ;; don't ask about fake addresses
       bbdb-ignore-some-messages-alist
       '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))))

    (bbdb-initialize 'gnus 'message)

    (hook-fn 'bbdb-mode-hook
      (local-set-key (kbd "j") 'bbdb-next-record)
      (local-set-key (kbd "k") 'bbdb-prev-record)
      (local-set-key (kbd "l") 'bbdb-next-field)
      (local-set-key (kbd "h") 'bbdb-prev-field))))

;; `google-this' provides a few simple commands for performing google searches.
(use-package google-this
  :ensure   t
  :commands google-this
  :diminish google-this-mode
  :init     (bind-key* "M-s" 'google-this)
  :config   (google-this-mode +1))

;; `w3m' bindings for Emacs. w3m is a command-line web browser.
(use-package w3m
  :ensure   t
  :commands
  (w3m
   w3m-find-file
   w3m-browse-url)
  :init
  (progn
    (setq browse-url-browser-function 'w3m-browse-url)

    (declare-modal-executor w3m
      :command w3m
      :bind "M-W"
      :restore-bindings '("M-W" "M-E"))

    (defun cb:w3m-browse-url-as-help (url)
      "Browse the given URL in a help window."
      (interactive
       (list
        (read-string "Go to URL: "
                     (thing-at-point-url-at-point)
                     t)))
      (with-window-restore
        (let ((win (or (--first-window (with-current-buffer (window-buffer it)
                                         (derived-mode-p 'w3m-mode)))
                       (split-window))))
          (select-window win)
          (w3m-browse-url url))
        (local-set-key (kbd "q") (command (restore)))))

    (defun cb:w3m-browse-dwim (url)
      "Browse to URL, ensuring it begins with http:// as required by w3m."
      (interactive
       (list
        (read-string "Go to URL: "
                     (thing-at-point-url-at-point)
                     t)))
      (with-window-restore
        (w3m-browse-url
         (if (s-starts-with? "http://" url)
             url
           (concat "http://" url)))
        (local-set-key (kbd "q") (command (restore)))))

    (when (true? cb:use-vim-keybindings?)
      (bind-key* "M-e" 'cb:w3m-browse-dwim)))

  :config
  (progn
    (bind-keys
      :map w3m-mode-map
      "z t" 'evil-scroll-line-to-top
      "z b" 'evil-scroll-line-to-bottom
      "z z" 'evil-scroll-line-to-center
      "C-f" 'evil-scroll-page-down
      "C-b" 'evil-scroll-page-up
      "y"   'evil-yank
      "p"   'evil-paste-after
      "/"   'evil-search-forward
      "?"   'evil-search-backward
      "n"   'evil-search-next
      "N"   'evil-search-previous)

    (hook-fn 'w3m-mode-hook
      (buffer-face-set
       `(:family ,(serif-font) :height 130)))))

;; `erc' is an Emacs IRC client.
(use-package erc
  :defer t
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

(provide 'cb-net)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-net.el ends here
