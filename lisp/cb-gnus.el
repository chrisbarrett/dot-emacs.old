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
    ;; Load evil-mode if it is available.
    (require 'evil nil 'no-error)
    (after 'evil

      (hook-fn 'gnus-group-mode-hook
        (local-set-key (kbd "j") 'gnus-group-next-unread-group)
        (local-set-key (kbd "k") 'gnus-group-prev-unread-group)
        (local-set-key (kbd "n") 'gnus-group-jump-to-group))

      (hook-fn 'gnus-summary-mode-hook
        (local-set-key (kbd "j") 'gnus-summary-next-unread-article)
        (local-set-key (kbd "k") 'gnus-summary-prev-unread-article)
        (local-set-key (kbd "n") 'gnus-summary-jump-to-group))

      (hook-fn 'gnus-article-mode-hook
        (local-set-key (kbd "j") 'evil-next-line)
        (local-set-key (kbd "k") 'evil-previous-line)
        (local-set-key (kbd "C-f") 'evil-scroll-page-down)
        (local-set-key (kbd "C-b") 'evil-scroll-page-up)
        (local-set-key (kbd "z z") 'evil-scroll-line-to-center)
        (local-set-key (kbd "z t") 'evil-scroll-line-to-top)
        (local-set-key (kbd "z b") 'evil-scroll-line-to-bottom))

      (hook-fn 'gnus-browse-mode-hook
        (local-set-key (kbd "j") 'gnus-browse-next-group)
        (local-set-key (kbd "k") 'gnus-browse-prev-group)
        (local-set-key (kbd "/") 'evil-search-forward)
        (local-set-key (kbd "n") 'evil-search-next)
        (local-set-key (kbd "N") 'evil-search-previous)))))

(provide 'cb-gnus)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-gnus.el ends here
