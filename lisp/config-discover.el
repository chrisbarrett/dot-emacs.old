;;; config-discover.el --- Configuration for discover.el

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

;; Configuration for discover.el

;;; Code:

(cb:install-package 'discover t)
(global-discover-mode +1)

(after 'evil
  (evil-set-initial-state 'makey-key-mode 'emacs))

(defmacro search:fn-with-thing-at-point (binding &rest body)
  "Bind BINDING to the symbol at point around BODY forms."
  (declare (indent 1))
  `(lambda ()
     (interactive)
     (let ((,binding
            (let ((s (or (current-region) (thing-at-point 'symbol))))
              (when s
                (substring-no-properties s)))))
       ,@body)))

(defun search:read-string (source-name &optional default)
  "Read a query for SOURCE-NAME with an optional DEFAULT."
  (let ((prompt (if default
                    (format "%s (default: %s): " source-name default)
                  (format "%s: " source-name))))
    (read-string prompt nil t default)))


(discover-add-context-menu
 :context-menu
 `(cb-search
   (description "Search commands")
   (actions

    ("Dictionary"
     ("d" "Dictionary"
      ,(search:fn-with-thing-at-point q
         (dictionary-search (search:read-string "Dictionary" q)))))

    ("Org"
     ("o" "Org files" org-search-view))

    ("Internet"
     ("g" "Google Web"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "https://www.google.com/search?q="
                  (url-hexify-string (search:read-string "Google Web" q))))))

     ("i" "Google Images"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "https://www.google.co.nz/search?tbm=isch&q="
                  (url-hexify-string (search:read-string "Google Images" q))))))

     ("y" "YouTube"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "http://www.youtube.com/results?search_query="
                  (url-hexify-string (search:read-string "YouTube" q))))))

     ("w" "Wikipedia"
      ,(search:fn-with-thing-at-point q
         (browse-url
          (concat "http://en.wikipedia.org/w/index.php?search="
                  (url-hexify-string (search:read-string "Wikipedia" q)))))))

    ("System"
     ("i" "info" helm-info-at-point)
     ("m" "manpages"
      ,(search:fn-with-thing-at-point q
         (require 'helm-man)
         (helm :sources 'helm-source-man-pages
               :buffer "*Helm man woman*"
               :input q)))))))

(bind-key* "M-s" 'makey-key-mode-popup-cb-search)

(provide 'config-discover)

;;; config-discover.el ends here
