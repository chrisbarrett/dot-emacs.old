;;; cb-indentation --- Rigid indentation mode.

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

;; Rigid indentation mode.

;;; Code:

(define-minor-mode rigid-indentation-mode
  "Minor mode where re-indentation is applied before and after
  each editing action."
  nil nil nil)

(defadvice self-insert-command (around indent-rigidly-on-insert activate)
  "Indent aggressively when `rigid-indentation-mode' is active."
  (if (and (boundp 'rigid-indentation-mode)
           rigid-indentation-mode
           (not (or (active-minibuffer-window) cursor-in-echo-area)))
      (progn (indent-according-to-mode)
             ad-do-it
             (indent-according-to-mode))
    ad-do-it))

(provide 'cb-indentation)

;;; cb-indentation.el ends here
