;;; cb-paredit --- Paredit extensions.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

;; Paredit extensions.

;;; Code:

(require 'paredit)

(defun cb:paredit-next-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (cb:paredit-forward))

(defun cb:paredit-previous-top-level-form ()
  (interactive)
  (if (ignore-errors (paredit-backward-up) t)
      (while (ignore-errors (paredit-backward-up) t))
    (paredit-backward)))

(defun cb:paredit-forward ()
  "Move to the beginning of the next item in the sexp."
  (interactive)
  (if (and (not (paredit-in-string-p))
           (save-excursion
             (ignore-errors
               (forward-sexp)
               (forward-sexp)
               t)))
      (progn
        (forward-sexp)
        (forward-sexp)
        (backward-sexp))
    (paredit-forward)))

(defun cb:paredit-forward-slurp-sexp-neatly ()
  (interactive)
  (save-excursion
    (cond
     ;; Fail if we're in a comment.
     ((or (paredit-in-comment-p)
          (paredit-in-char-p))
      (error "Invalid context for slurping S-expressions"))
     ;; Slurp strings
     ((paredit-in-string-p)
      (paredit-forward-slurp-into-string))
     ;; Else slurp sexp.
     ((save-excursion
        (paredit-forward-up)
        (paredit-backward-down)
        (paredit-forward-slurp-sexp)
        (just-one-space)))))
  ;; Cleanup.
  (when (not (save-excursion
               (ignore-errors
                 (backward-sexp) t)))
    (delete-horizontal-space)))

(provide 'cb-paredit)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-paredit.el ends here
