;;; cb-google --- Commands for querying Google

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

;; Commands for querying Google

;;; Code:

(defconst cb:google-search-uri "http://www.google.com/search?ie=utf-8&oe=utf-8&q=")

(defun cb:read-string-with-default (prompt default)
  "Like `read-string', supplying a default value."
  (let ((bracketed (when default (concat " (" default ")"))))
    (read-string (concat prompt bracketed ": ") nil nil default)))

(defun current-region ()
  "Return the text in the current region."
  (when (region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun google/search (query)
  "Perform a google search for QUERY string.
If called interactively, searches for either the current region
or the symbol at point."
  (interactive
   (list (or (current-region)
             (cb:read-string-with-default "Google" (thing-at-point 'symbol)))))
  (let ((url (concat cb:google-search-uri (url-hexify-string query))))
    (if (executable-find "w3m")
        (w3m-browse-url url)
      (browse-url url))))

(provide 'cb-google)

;;; cb-google.el ends here
