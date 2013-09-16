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
(require 'cb-typefaces)

;; Disable smartparens in twittering-mode.
(after 'smartparens
  (hook-fn 'twittering-mode-hook
    (smartparens-mode -1)))

;; `twittering-mode' is an Emacs twitter client.
(use-package twittering-mode
  :ensure t
  :commands twit
  :config
  (progn
    (setq twittering-use-master-password t
          twittering-icon-mode t
          twittering-convert-fix-size 32
          twittering-use-icon-storage t
          twittering-timer-interval (* 3 60)
          twittering-url-show-status nil
          twittering-edit-skeleton 'inherit-any)

    ;;; Custom faces for tweets.

    (defface cbtw:status-text
      `((t (:family ,(sans-serif-font))))
      "Face for the text component of a tweet"
      :group 'twittering-mode)

    (defface cbtw:status-retweet
      `((t (:italic t :family ,(sans-serif-font) :foreground "#6c71c4")))
      "Face for the retweet info in tweet"
      :group 'twittering-mode)

    (defface cbtw:timestamp
      '((((background dark))  (:foreground "gray50"))
        (((background light)) (:foreground "gray70"))
        (t (:foreground "#b58900" :height 110)))
      "Face for the timestamp in a tweet."
      :group 'twittering-mode)

    (setq twittering-status-format
          (concat
           "%i %s\n\n"
           "%FILL[  ]{%FACE[cbtw:status-text]{%T}}\n\n"
           "  %FACE[cbtw:timestamp]{%@}"
           " %FACE[cbtw:status-retweet]{%r%R}\n\n"))))

(provide 'cb-twittering)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-twittering.el ends here
