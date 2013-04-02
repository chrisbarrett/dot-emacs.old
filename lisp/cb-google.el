;;; cb-google.el

(cb:require-package 'w3m)

(defconst cb:google-search-uri "http://www.google.com/search?ie=utf-8&oe=utf-8&q=")

(defun cb:read-string-with-default (prompt default)
  "Like `read-string', supplying a default value."
  (let ((bracketed (when default (concat " (" default ")"))))
    (read-string (concat prompt bracketed ": ") nil nil default)))

(defun current-region ()
  (when (region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun google/search (query)
  "Perform a google search for the given query.
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
