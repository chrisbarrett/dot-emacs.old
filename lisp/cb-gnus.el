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
          gnus-save-newsrc-file nil
          gnus-read-newsrc-file nil
          gnus-startup-file (concat cb:etc-dir "gnus"))

    (hook-fn 'gnus-article-mode-hook
      "Use a sans-serif font for gnus-article-mode."
      (when (equal major-mode 'text-mode)
        (buffer-face-set `(:family ,(sans-serif-font) :height 120))))

    (after 'evil
      ;; Group mode
      (define-key gnus-group-mode-map (kbd "j") 'gnus-group-next-unread-group)
      (define-key gnus-group-mode-map (kbd "k") 'gnus-group-prev-unread-group)
      (define-key gnus-group-mode-map (kbd "n") 'gnus-group-jump-to-group)
      ;; Summary mode
      (define-key gnus-summary-mode-map (kbd "j") 'gnus-summary-next-unread-article)
      (define-key gnus-summary-mode-map (kbd "k") 'gnus-summary-prev-unread-article)
      (define-key gnus-summary-mode-map (kbd "n") 'gnus-summary-jump-to-group)
      ;; Article mode
      (define-key gnus-article-mode-map (kbd "j") 'evil-next-line)
      (define-key gnus-article-mode-map (kbd "k") 'evil-previous-line)
      (define-key gnus-article-mode-map (kbd "C-f") 'evil-scroll-page-down)
      (define-key gnus-article-mode-map (kbd "C-b") 'evil-scroll-page-up)
      (define-key gnus-article-mode-map (kbd "z z") 'evil-scroll-line-to-center)
      (define-key gnus-article-mode-map (kbd "z t") 'evil-scroll-line-to-top)
      (define-key gnus-article-mode-map (kbd "z b") 'evil-scroll-line-to-bottom)

      ;; Browse mode
      (after 'gnus-srvr
        (define-key gnus-browse-mode-map (kbd "j") 'gnus-browse-next-group)
        (define-key gnus-browse-mode-map (kbd "k") 'gnus-browse-prev-group)
        (define-key gnus-browse-mode-map (kbd "/") 'evil-search-forward)
        (define-key gnus-browse-mode-map (kbd "n") 'evil-search-next)
        (define-key gnus-browse-mode-map (kbd "N") 'evil-search-previous)))

    (hook-fn 'gnus-summary-mode-hook
      "Use a summary style better suited to RSS."
      (when (s-matches? (rx bol "nrss:" (* nonl)) gnus-newsgroup-name)
        (setq-local gnus-show-threads nil)
        (setq-local gnus-article-sort-functions 'gnus-article-sort-by-date)
        (setq-local gnus-use-adaptive-scoring nil)
        (setq-local gnus-use-scoring t)
        (setq-local gnus-score-find-score-files-function 'gnus-score-find-single)
        (setq-local gnus-summary-line-format "%U%R%z%d %I%(%[ %s %]%)\n")))))

(provide 'cb-gnus)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-gnus.el ends here
