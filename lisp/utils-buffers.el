;;; utils-buffers.el --- Utilities for managing buffers and windows

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

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

;; Utilities for managing buffers and windows

;;; Code:

(require 'utils-common)
(require 'cl-lib)

(cl-defmacro --filter-buffers (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-filter-buffers'"
  `(--filter (with-current-buffer it ,pred-form) ,bufs))

(cl-defmacro --map-buffers (form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-map-buffers'"
  `(--map (with-current-buffer it ,form) ,bufs))

(cl-defmacro --first-buffer (pred-form &optional (bufs '(buffer-list)))
  "Anaphoric form of `-first-buffer'"
  `(--first (with-current-buffer it ,pred-form) ,bufs))

(defalias '-first-window 'get-window-with-predicate)

(defmacro --first-window (pred-form)
  "Anaphoric form of `-first-window'.
Find the first window where PRED-FORM is not nil."
  `(-first-window (lambda (it) ,pred-form)))

(cl-defun expose-buffers
    (buffers &optional (sort-fn (-on 'string< 'buffer-file-name)))
  "Show an Exposé-style arrangement of BUFFERS."
  (when buffers
    (delete-other-windows)
    (let* ((live (-filter 'buffer-live-p (-sort sort-fn buffers)))
           (padded (if (cl-evenp (length live)) live (nreverse (cons nil (nreverse live)))))
           (bs (apply '-zip (-partition (/ (length padded) 2) padded))))

      (when live
        (switch-to-buffer (caar bs) t))

      ;; Split sensibly for 2-up view, otherwise show a grid.
      (cond
       ((= 1 (length bs))
        (-when-let (bot (cdar bs))
          (select-window (split-window-sensibly))
          (switch-to-buffer bot)))

       (t
        (-each (cdr bs)
          (lambda+ ((top . bot))
            (select-window (split-window-horizontally))
            (switch-to-buffer top)
            (balance-windows)))
        (-each bs
          (lambda+ ((top . bot))
            (select-window (get-buffer-window top))
            (when bot
              (select-window (split-window-vertically))
              (switch-to-buffer bot)))))))))


(provide 'utils-buffers)

;;; utils-buffers.el ends here
