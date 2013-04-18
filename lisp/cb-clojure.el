;;; cb-clojure --- Commands for working with Clojure and nREPL.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Commands for working with Clojure and nREPL.

;;; Code:

(require 'nrepl)
(require 'clojure-mode)
(require 'dash)
(require 's)

(defadvice nrepl-switch-to-repl-buffer (after insert-at-end-of-nrepl-line activate)
  "Enter insertion mode at the end of the line when switching to nrepl."
  (when (and (boundp 'evil-mode)
             evil-mode
             (not (evil-insert-state-p)))
    (evil-append-line 0)))

(defadvice back-to-indentation (around move-to-nrepl-bol activate)
  "Move to position after prompt."
  (if (equal major-mode 'nrepl-mode)
      (nrepl-bol)
    ad-do-it))

(defun cb:switch-to-nrepl ()
  "Start nrepl or switch to an existing nrepl buffer."
  (interactive)
  (if-let (buf (get-buffer "*nrepl*"))
    (nrepl-switch-to-repl-buffer buf)
    (nrepl-jack-in)))

(defun cb:switch-to-last-clj-buffer ()
  "Switch to the last active clojure buffer."
  (interactive)
  (when-let (buf (cb:last-buffer-for-mode 'clojure-mode))
    (pop-to-buffer buf)))

(defun cb:eval-last-clj-buffer ()
  "Evaluate that last active clojure buffer without leaving the repl."
  (interactive)
  (when-let (buf (cb:last-buffer-for-mode 'clojure-mode))
    (with-current-buffer buf
      (nrepl-eval-buffer))))

(provide 'cb-clojure)

;;; cb-clojure.el ends here
