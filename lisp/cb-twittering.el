;;; cb-twittering.el --- Configuration for Twittering mode

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130915.2226

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

;; Configuration for Twittering mode

;;; Code:

(require 'use-package)

;; `twittering-mode' is an Emacs twitter client.
(use-package twittering-mode
  :ensure t
  :commands twit
  :config
  (setq twittering-use-master-password t
        twittering-icon-mode t
        twittering-use-icon-storage t
        twittering-timer-interval (* 3 60)
        twittering-url-show-status nil
        twittering-edit-skeleton 'inherit-any

        twittering-status-format
        "%i %s,  %@:\n%FILL[  ]{%T}\n%r%R"))

(provide 'cb-twittering)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-twittering.el ends here
