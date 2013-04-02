;;; cb-xml

;;; Formatting

(defun cb:reformat-xml-in-region (begin end)
  (save-excursion
    (let ((end end))
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char)
        (insert "\n")
        (incf end))
      (indent-region begin end))))

(defun cb:reformat-xml ()
  "Insert newlines and indent XML. Operates on region, or the whole buffer if no region is defined."
  (interactive)
  (save-excursion
    (let ((start (or (ignore-errors (region-beginning))
                     (point-min)))
          (end   (or (ignore-errors (region-end))
                     (point-max))))
      (cb:reformat-xml-in-region start end))))

(provide 'cb-xml)
