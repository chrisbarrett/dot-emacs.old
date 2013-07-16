;;; cb-gnus.el --- Configuration for gnus

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130714.2332

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

;; Configuration for gnus

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-typefaces)

(use-package gnus
  :commands gnus
  :defer t
  :init
  (when (truthy? cb:use-vim-keybindings?)
    (bind-key* "M-Y" 'gnus))
  :config
  (progn
    (setq gnus-treat-fill t
          gnus-always-read-dribble-file t
          gnus-save-newsrc-file nil
          gnus-read-newsrc-file nil
          gnus-startup-file (concat cb:etc-dir "gnus"))

    (hook-fn 'gnus-article-mode-hook
      "Use a sans-serif font for gnus-article-mode."
      (setq-local line-spacing 0.25)
      (buffer-face-set `(:family ,(serif-font) :height 140))
      (set-face-font 'gnus-header-name (sans-serif-font))
      (set-face-font 'gnus-header-subject (sans-serif-font))
      (set-face-font 'gnus-header-content (sans-serif-font))
      (set-face-font 'gnus-button (sans-serif-font)))

    (hook-fn 'gnus-summary-mode-hook
      "Use a summary style better suited to RSS."
      (when (s-matches? (rx bol "nrss:" (* nonl)) gnus-newsgroup-name)
        (setq-local gnus-show-threads nil)
        (setq-local gnus-article-sort-functions 'gnus-article-sort-by-date)
        (setq-local gnus-use-adaptive-scoring nil)
        (setq-local gnus-use-scoring t)
        (setq-local gnus-score-find-score-files-function 'gnus-score-find-single)
        (setq-local gnus-summary-line-format "%U%R%z%d %I%(%[ %s %]%)\n")))

    ;; Set evil-style motion keys.
    (after 'evil

      (after 'gnus-group
        ;; gnus-group-mode
        (bind-key "j" 'gnus-group-next-group gnus-group-mode-map)
        (bind-key "k" 'gnus-group-prev-group gnus-group-mode-map)
        (bind-key "n" 'gnus-group-jump-to-group gnus-group-mode-map))

      (after 'gnus-sum
        ;; gnus-summary-mode
        (bind-key "j" 'gnus-summary-next-article gnus-summary-mode-map)
        (bind-key "k" 'gnus-summary-prev-article gnus-summary-mode-map)
        (bind-key "n" 'gnus-summary-jump-to-group gnus-summary-mode-map))

      (after 'gnus-art
        ;; gnus-article-mode
        (bind-key "j" 'next-line gnus-article-mode-map)
        (bind-key "k" 'previous-line gnus-article-mode-map)
        (bind-key "C-n" 'gnus-summary-next-article gnus-article-mode-map)
        (bind-key "C-p" 'gnus-summary-prev-article gnus-article-mode-map)
        (bind-key "C-f" 'evil-scroll-page-down gnus-article-mode-map)
        (bind-key "C-b" 'evil-scroll-page-up gnus-article-mode-map)
        (bind-key "z z" 'evil-scroll-line-to-center gnus-article-mode-map)
        (bind-key "z t" 'evil-scroll-line-to-top gnus-article-mode-map)
        (bind-key "z b" 'evil-scroll-line-to-bottom gnus-article-mode-map))

      (after 'gnus-srvr
        ;; gnus-server-mode
        (bind-key "j" 'evil-next-line gnus-server-mode-map)
        (bind-key "k" 'evil-previous-line gnus-server-mode-map)
        ;; gnus-browse-mode
        (bind-key "j" 'gnus-browse-next-group gnus-browse-mode-map)
        (bind-key "k" 'gnus-browse-prev-group gnus-browse-mode-map)
        (bind-key "C-f" 'evil-scroll-page-down gnus-browse-mode-map)
        (bind-key "C-b" 'evil-scroll-page-up gnus-browse-mode-map)
        (bind-key "/" 'evil-search-forward gnus-browse-mode-map)
        (bind-key "n" 'evil-search-next gnus-browse-mode-map)
        (bind-key "N" 'evil-search-previous gnus-browse-mode-map)))))

(provide 'cb-gnus)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-gnus.el ends here
