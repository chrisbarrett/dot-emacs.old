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

(defun xml/reformat ()
  "Insert newlines and indent XML. Operates on region, or the whole buffer if no region is defined."
  (interactive)
  (save-excursion
    (let ((start (or (ignore-errors (region-beginning))
                     (point-min)))
          (end   (or (ignore-errors (region-end))
                     (point-max))))
      (cb:reformat-xml-in-region start end))))

;;; Hooks

(defun cb:xml-file? ()
  (or (s-ends-with? ".xml" (buffer-file-name))
      (s-starts-with? "<?xml " (buffer-string))))

(defun cb:on-find-file ()
  (when (cb:xml-file?)
    (nxml-mode)))

(defun cb:on-nxml-mode ()
  (local-set-key (kbd "M-q") 'xml/reformat))

(add-hook 'nxml-mode-hook 'cb:on-nxml-mode)
(add-hook 'find-file-hook 'cb:on-find-file)

(provide 'cb-xml)
