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

(defvar cbs:search-methods nil
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
          (-when-let (s (or (current-region) (thing-at-point 'symbol)))
            (substring-no-properties s)))
        (m
         (read-option
          "*Select Search*"
          'cbs-search-method-key 'cbs-search-method-name
          (->> cbs:search-methods
            ;; Use methods without a predicate or where the
            ;; predicate returns non-nil.
            (--filter
             (-if-let (p (cbs-search-method-pred it))
                 (funcall p)
               t))
            ;; Drop duplicated options.
            (-uniq-by (π cbs-search-method-name cbs-search-method-key))
            ;; Sort by key.
            (-sort (-on 'string< (C s-upcase cbs-search-method-key)))))))
    (funcall (cbs-search-method-func m) default-search-term)))

(bind-key* "M-s" 'cbs-search)

;;; Search methods

(cbs-define-search-method
 :name "Dictionary"
 :key "d"
 :command
 (lambda (q)
   (dictionary-search (cbs-read "Dictionary" q))))

(cbs-define-search-method
 :name "Web Search"
 :key "s"
 :command
 (lambda (q)
   (browse-url
    (concat "https://duckduckgo.com/?q="
            (url-hexify-string (cbs-read "Duck Duck Go" q))))))

(cbs-define-search-method
 :name "Image Search"
 :key "i"
 :command
 (lambda (q)
   (browse-url
    (concat "https://www.google.co.nz/search?tbm=isch&q="
            (url-hexify-string (cbs-read "Google Images" q))))))

(cbs-define-search-method
 :name "YouTube"
 :key "y"
 :command
 (lambda (q)
   (browse-url
    (concat "http://www.youtube.com/results?search_query="
            (url-hexify-string (cbs-read "YouTube" q))))))

(cbs-define-search-method
 :name "Wikipedia"
 :key "w"
 :command
 (lambda (q)
   (browse-url
    (concat "http://en.wikipedia.org/w/index.php?search="
            (url-hexify-string (cbs-read "Wikipedia" q))))))

(cbs-define-search-method
 :name "BBDB"
 :key "b"
 :command
 (lambda (_)
   (call-interactively 'bbdb)))

(cbs-define-search-method
 :name "Man Page"
 :key "m"
 :command
 (lambda (q)
   (require 'helm-man)
   (helm :sources 'helm-source-man-pages
         :buffer "*Helm man woman*"
         :input q)))

(cbs-define-search-method
 :name "Notmuch (mail)"
 :key "n"
 :when (lambda () (executable-find "notmuch"))
 :command
 (lambda (q)
   (notmuch-search (cbs-read "Mail" q))))

(cbs-define-search-method
 :name "GitHub"
 :key "g"
 :command
 (lambda (q)
   (browse-url
    (concat "https://github.com/search?q="
            (url-hexify-string (cbs-read "GitHub Search" q))))))

(provide 'cb-search)

;; Local Variables:
;; End:

;;; cb-search.el ends here
