;;; config-bbdb.el --- Configuration for BBDB

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

;; Configuration for BBDB

;;; Code:

(require 'utils-common)

(cb:install-package 'bbdb)

(custom-set-variables

 '(osxb-import-with-timer (equal system-type 'darwin))

 '(bbdb-file (concat user-dropbox-directory ".bbdb"))
 '(bbdb-use-popup t)
 '(bbdb-pop-up-window-size 0.5)
 '(bbdb-mua-popup-window-size 4)
 '(bddb-popup-target-lines 1)
 '(bbdb-offer-save 1)
 '(bbdb-always-add-address t)
 '(bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)
 '(bbdb-ignore-some-messages-alist
   '(("From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
 '(bbdb-complete-name-allow-cycling t)
 '(bbdb-use-alternate-names t)
 '(bbdb-elided-display t)
 '(bbdb-electric t)
 '(bbdb-completion-type nil)
 '(bbdb-dwim-net-address-allow-redundancy t)
 '(bbdb-quiet-about-name-mismatches 2)
 '(bbdb-canonicalize-redundant-nets-p t)
 '(bbbd-message-caching-enabled t))

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'message-mode-hook 'bbdb-insinuate-mail)

(after '(bbdb evil)
  (define-key bbdb-mode-map (kbd "j") 'bbdb-next-record)
  (define-key bbdb-mode-map (kbd "k") 'bbdb-prev-record)
  (define-key bbdb-mode-map (kbd "l") 'bbdb-next-field)
  (define-key bbdb-mode-map (kbd "h") 'bbdb-prev-field))

(provide 'config-bbdb)

;;; config-bbdb.el ends here
