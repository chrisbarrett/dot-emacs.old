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

;; Configuration for gnus. Gnus is the built-in newsreader, which also supports
;; email.

;;; Code:

(require 'use-package)
(require 'cb-foundation)
(require 'cb-typefaces)
(require 'async)

;; Define a global key binding for starting gnus. It will run unplugged, which
;; should speed things up.
;;
;; Note that you will still need to add individual servers to be agentized - use
;; <J a> in the server buffer to add servers to the agent list.

(declare-modal-executor show-gnus
  :bind "M-Y"
  :restore-bindings '("q")
  :command
  (with-window-restore
    (-if-let (b (get-buffer "*Group*"))
      (switch-to-buffer b)
      (gnus-unplugged))
    ;; Rebind `gnus-group-exit' to <Q>
    (local-set-key (kbd "Q") (command
                              (gnus-group-exit)
                              (restore)))))

;; Fetch news in the background every 2 minutes.

(defvar cb-gnus:fetch-interval (* 2 60)
  "The frequency in seconds at which new articles will be fetched.")

(defvar cb-gnus:agent-batch-timer
  (unless noninteractive
    (run-with-timer 0.5 cb-gnus:fetch-interval 'cb-gnus:agent-batch-fetch))
  "Timer to periodically fetch news using the gnus agent.")

(defun cb-gnus:agent-batch-fetch ()
  "Download news with the gnus agent."
  (let ((proc (start-process
               "gnus agent" nil
               "emacs" "-batch" "-l" user-init-file "-f" "gnus-agent-batch")))
    ;; Kill process if it's open for more than 5 minutes.  That should be
    ;; plenty of time to fetch articles, even for first sync.
    (run-with-timer (* 5 60) nil (lambda (p)
                                   (when (process-live-p p)
                                     (kill-process p)))
                    proc)))

;; Find number of unread emails and update the mode-line.

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

;; Update the unread mail count for all gnus hooks.
(after 'gnus
  (let ((hooks (--filter-atoms (s-matches? "^gnus-.*-hook$" (symbol-name it)))))
    (eval
     `(hook-fns ',hooks
        ;; Rebind hooks to prevent infinite loops.
        (ignore-errors
          (let ,hooks
            (cb-gnus:update-modeline-unread
             (cb-gnus:sum-unread (cb-gnus:scrape-group-buffer-for-news)))))))))

;; Periodically refresh the *group* buffer and update the modeline.
;;
;; Every minute, wait for an idle opportunity to perform the refresh.

(defvar cb-gnus:idle-checker-delay 60
  "The rough frequency in seconds at which to check gnus for changes.")

(defvar cb-gnus:modeline-refresh-timer nil
  "A handle to the current gnus refresh timer.")

(defun cb-gnus:make-idle-checker ()
  "Create a timer that will wait for an opportunity to refresh gnus.
After updating the group"
  (run-with-timer
   cb-gnus:idle-checker-delay
   nil
   (lambda ()
     ;; Update at the next opportunity.
     (run-with-idle-timer
      7 nil
      (lambda ()
        (save-window-excursion
          ;; Update gnus only if it's running (i.e. the *group* buffer exists)
          (when (--first-buffer (derived-mode-p 'gnus-group-mode))
            (noflet ((message (&rest args) nil)
                     ;; HACK: gnus will sometimes prompt for things. I don't
                     ;; care, just YES.
                     (Y-or-n-p (&rest args) t))
              (gnus-group-get-new-news)))
          ;; Recur and update the timer var so there's a
          ;; cancelable handle somewhere.
          (setq cb-gnus:modeline-refresh-timer
                (cb-gnus:make-idle-checker))))))))

;; `gnus-agent' provides offline syncing functionality for gnus.  Configure the
;; agent to provide fast IMAP access.
(use-package gnus-agent
  :defer t
  :config
  (progn
    (add-hook 'gnus-select-article-hook 'gnus-agent-fetch-selected-article)
    (setq gnus-agent-directory (f-join cb:etc-dir "gnus-agent")
          ;; Send mail immediately, rather than queuing.
          gnus-agent-queue-mail nil
          gnus-agent-auto-agentize-methods '(nntp nnimap))))

;; Configure gnus.

(use-package gnus
  :commands gnus
  :defer t
  :config
  (progn

    (setq cb-gnus:modeline-refresh-timer (unless noninteractive
                                             (cb-gnus:make-idle-checker))
          gnus-always-read-dribble-file t
          gnus-startup-file (f-join cb:etc-dir "gnus")
          gnus-current-startup-file (f-join cb:etc-dir "gnus")
          gnus-dribble-directory (f-join cb:etc-dir "gnus-dribble"))

    ;; Custom faces

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
        (setq-local gnus-summary-line-format "%U%R%z%d %I%(%[ %s %]%)\n")))))

;; Configure evil-style motion keys in the various gnus modes.
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
      "N" 'evil-search-previous)))

(provide 'cb-gnus)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-gnus.el ends here
