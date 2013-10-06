;;; cb-search.el --- Search interface

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130909.0446

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

;; Search interface from Emacs.

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'cb-lib)
(require 'cb-colour)
(require 'bind-key)

(defun cbs-search-method
  (name key search-func &optional pred)
  (list name key search-func pred))
(cl-defun cbs-search-method-name ((n _ _ _)) n)
(cl-defun cbs-search-method-key  ((_ k _ _)) k)
(cl-defun cbs-search-method-func ((_ _ f _)) f)
(cl-defun cbs-search-method-pred ((_ _ _ p)) p)

(defun cbs:read-query (source-name &optional default)
  "Read a query for SOURCE-NAME with an optional DEFAULT."
  (let ((prompt (if default
                    (format "%s (default: %s): " source-name default)
                  (format "%s: " source-name))))
    (read-string prompt nil t default)))

(defvar cbs:search-methods
  (list
   (cbs-search-method
    "Google Search" "s"
    (lambda (q)
      (browse-url
       (concat "http://www.google.com/search?ie=UTF-8&oe=UTF-8&q="
               (url-hexify-string q)))))
   (cbs-search-method
    "Google Images" "i"
    (lambda (q)
      (browse-url
       (concat "https://www.google.co.nz/search?tbm=isch&q="
               (url-hexify-string q)))))
   (cbs-search-method
    "YouTube" "y"
    (lambda (q)
      (browse-url
       (concat "http://www.youtube.com/results?search_query="
               (url-hexify-string q)))))
   (cbs-search-method
    "Wikipedia" "w"
    (lambda (q)
      (browse-url
       (concat "http://en.wikipedia.org/w/index.php?search="
               (url-hexify-string q))))))
  "List of search methods.")

(cl-defun cbs-define-search-method (&rest spec)
  "Define a new search method.
NAME is the user-facing description.
KEY is used to select it from the menu.
SEARCH-FUNC is a unary function that will be passed the query string.
PRED is a predicate to determine whether search method is currently available.

\(fn name key search-func &optional pred)"
  (add-to-list 'cbs:search-methods (apply 'cbs-search-method spec)))

(defun cbs-search ()
  "Submit a query to a selected search provider."
  (interactive)
  (message "Select search method")
  (let ((default (or (current-region) (thing-at-point 'symbol)))
        (m (read-option
            " *Select Search*"
            'cbs-search-method-key 'cbs-search-method-name
            (->> cbs:search-methods
              ;; Use methods without a predicate or where the
              ;; predicate returns non-nil.
              (--filter
               (-if-let (p (cbs-search-method-pred it))
                   (funcall p)
                 t))
              (-uniq)
              (-sort (-on 'string< 'cbs-search-method-key))))))
    (funcall (cbs-search-method-func m)
             (cbs:read-query (cbs-search-method-name m)
                             default))))

(bind-key* "M-s" 'cbs-search)

(provide 'cb-search)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-search.el ends here
