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
(require 'async)
(autoload 'gnus-dribble-read-file "gnus-start")

(defvar gnus-startup-file (f-join cb:etc-dir "gnus"))
(defvar gnus-current-startup-file (f-join cb:etc-dir "gnus"))
(defvar gnus-dribble-directory (f-join cb:etc-dir "gnus-dribble"))


(unless noninteractive
  (after 'personal-config

  ;;;; Asynchronous gnus updates

    ;; Use a timer to periodically cache gnus news in the background. Once an
    ;; async process is launched, a timer is also started to enforce a
    ;; timeout. This is needed because gnus will sometimes hang when fetching.

    (defvar gnus-async-refresh-rate 60)
    (defvar gnus-async-timeout 60)
    (defvar gnus-async-refresh-timer
      (run-with-timer
       nil
       gnus-async-refresh-rate
       (lambda ()
         (let ((proc (gnus-refresh-async)))
           (eval `(run-with-timer gnus-async-timeout nil
                                  (lambda ()
                                    (when (process-live-p ,proc)
                                      (let (kill-buffer-query-functions)
                                        (kill-process ,proc)
                                        (kill-buffer (process-buffer ,proc)))))))))))

    (defun cb-gnus:sum-unread (news-plist)
      "Return the number of unread emails in NEWS-PLIST."
      (-when-let (imap (second (assoc 'nnimap gnus-secondary-select-methods)))
        (->> news-plist
          (--filter (s-contains? imap (plist-get it :name)))
          (--map (plist-get it :unread-count))
          (-sum))))

    (defun cb-gnus:update-modeline-unread (count)
      (setq modeline-mail-indicator
            (when (plusp count)
              (format " %s unread" count))))

    (defun cb-gnus:scrape-group-buffer-for-news ()
      (-when-let (buf (get-buffer "*Group*"))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (cl-loop while (not (eobp))
                     for group = (gnus-group-name-at-point)
                     do (gnus-group-update-group group)
                     when group collect
                     (list :name group
                           :unread-count (gnus-group-unread group))
                     do (forward-line))))))

    (hook-fns (--filter-atoms (s-matches? "^gnus-.*-hook$" (symbol-name it)))
      (-when-let (sum (ignore-errors
                        (cb-gnus:sum-unread (cb-gnus:scrape-group-buffer-for-news))))
        (cb-gnus:update-modeline-unread sum)))

    (defun gnus-refresh-async ()
      "Spawn a background Emacs instance to download the latest news and emails."
      (interactive)
      (async-start

       `(lambda ()
          (message "Preparing environment...")
          (require 'gnus)
          (require 'cl-lib)
          (setq gnus-startup-file ,gnus-startup-file
                gnus-dribble-directory ,gnus-dribble-directory
                gnus-expert-user t
                gnus-current-startup-file ,gnus-current-startup-file
                gnus-always-read-dribble-file t
                gnus-select-method ',gnus-select-method
                gnus-secondary-select-methods ',gnus-secondary-select-methods)
          (message "Downloading news...")
          (gnus)
          (prog1
              ;; Get the unread counts of groups with new news.
              (cl-loop while (not (eobp))
                       initially (message "Collating unread items...")
                       for group = (gnus-group-name-at-point)
                       when group collect
                       (list :name group
                             :unread-count (gnus-group-unread group))
                       do (forward-line))
            (message "Saving dribble file...")
            (gnus-dribble-save)
            (gnus-batch-kill)
            (gnus-group-exit)
            (message "Finished.")))

       (lambda (news)
         ;; Notify user of new email.
         (when news
           (cb-gnus:update-modeline-unread (cb-gnus:sum-unread news)))
         ;; Update the group view.
         (-when-let (b (get-buffer "*Group*"))
           (when (buffer-live-p b)
             (gnus-dribble-read-file)
             (with-current-buffer b
               (gnus-group-list-groups)))))))))

(defun show-gnus ()
  "Start gnus.  If gnus is already running, switch to the groups buffer."
  (interactive)
  (-if-let (b (get-buffer "*Group*"))
    (switch-to-buffer b)
    (gnus)))

(use-package gnus
  :commands gnus
  :defer t
  :init
  (when (true? cb:use-vim-keybindings?)
    (bind-key* "M-Y" 'show-gnus))
  :config
  (progn

    (setq gnus-always-read-dribble-file t)

    ;;;; Custom faces

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
        (bind-keys
          :map gnus-group-mode-map
          "j" 'gnus-group-next-group
          "k" 'gnus-group-prev-group
          "n" 'gnus-group-jump-to-group))

      (after 'gnus-sum
        (bind-keys
          :map gnus-summary-mode-map
          "j" 'gnus-summary-next-article
          "k" 'gnus-summary-prev-article
          "n" 'gnus-summary-next-unread-article))

      (after 'gnus-art
        (bind-keys
          :map gnus-article-mode-map
          "j" 'next-line
          "k" 'previous-line
          "w" 'evil-forward-word-begin
          "e" 'evil-forward-word-end
          "b" 'evil-backward-word-begin
          "C-n" 'gnus-summary-next-article
          "C-p" 'gnus-summary-prev-article
          "C-f" 'evil-scroll-page-down
          "C-b" 'evil-scroll-page-up
          "z z" 'evil-scroll-line-to-center
          "z t" 'evil-scroll-line-to-top
          "z b" 'evil-scroll-line-to-bottom))

      (after 'gnus-srvr
        (bind-keys
          :map gnus-server-mode-map
          "j" 'evil-next-line
          "k" 'evil-previous-line)
        (bind-keys
          :map gnus-browse-mode-map
          "j" 'gnus-browse-next-group
          "k" 'gnus-browse-prev-group
          "C-f" 'evil-scroll-page-down
          "C-b" 'evil-scroll-page-up
          "/" 'evil-search-forward
          "n" 'evil-search-next
          "N" 'evil-search-previous)))))

(provide 'cb-gnus)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-gnus.el ends here
