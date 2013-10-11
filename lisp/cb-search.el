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

(cl-defun cbs-search-method
   (&key name key command
          (when (lambda () t))
          (unless (lambda () nil)))
  (list name key command
        `(lambda ()
           (and (funcall ',when)
                (not (funcall ',unless))))))

(cl-defun cbs-search-method-name ((n _ _ _)) n)
(cl-defun cbs-search-method-key  ((_ k _ _)) k)
(cl-defun cbs-search-method-func ((_ _ f _)) f)
(cl-defun cbs-search-method-pred ((_ _ _ p)) p)

(defun cbs-read (source-name &optional default)
  "Read a query for SOURCE-NAME with an optional DEFAULT."
  (let ((prompt (if default
                    (format "%s (default: %s): " source-name default)
                  (format "%s: " source-name))))
    (read-string prompt nil t default)))

(defvar cbs:search-methods
  (list
   (cbs-search-method
    :name "Google Search"
    :key "s"
    :command
    (lambda (q)
      (let ((helm-pattern q))
        (helm-google-suggest))))

   (cbs-search-method
    :name "Google Images"
    :key "i"
    :command
    (lambda (q)
      (browse-url
       (concat "https://www.google.co.nz/search?tbm=isch&q="
               (url-hexify-string (cbs-read "Google Images" q))))))

   (cbs-search-method
    :name "YouTube"
    :key "y"
    :command
    (lambda (q)
      (browse-url
       (concat "http://www.youtube.com/results?search_query="
               (url-hexify-string (cbs-read "YouTube" q))))))

   (cbs-search-method
    :name "Wikipedia"
    :key "w"
    :command
    (lambda (q)
      (browse-url
       (concat "http://en.wikipedia.org/w/index.php?search="
               (url-hexify-string (cbs-read "Wikipedia" q))))))

   (cbs-search-method
    :name "Man Page"
    :key "m"
    :command
    (lambda (q)
      (let ((helm-pattern q))
        (helm-man-woman nil)))))

  "The list of search methods used by `cbs-search'.")

(cl-defun cbs-define-search-method (&rest spec)
  "Define a new search method.
NAME is the user-facing description.
KEY is used to select it from the menu.
SEARCH-FUNC is a unary function that will be passed the query string.
PRED is a predicate to determine whether search method is currently available.

\(fn &key name key command when unless)"
  (add-to-list 'cbs:search-methods (apply 'cbs-search-method spec)))

(defun cbs-search ()
  "Submit a query to a selected search provider."
  (interactive)
  (message "Select search method")
  (let ((default-search-term
          (or (current-region) (thing-at-point 'symbol)))
        (m
         (read-option
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
            (-sort (-on 'string< (C s-upcase cbs-search-method-key)))))))
    (funcall (cbs-search-method-func m) default-search-term)))

(bind-key* "M-s" 'cbs-search)

(provide 'cb-search)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-search.el ends here
