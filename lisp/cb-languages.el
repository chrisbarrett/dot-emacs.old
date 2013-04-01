;;; cb-languages

(defun cb:auto-mode-on-match (mode &rest regexes)
  "Use the provided major mode for files matching the given regex."
  (--each regexes
    (add-to-list 'auto-mode-alist `(,it . ,mode))))

(provide 'cb-languages)
