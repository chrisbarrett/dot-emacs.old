;;; cb-json.el

(cb:require-package 'json-mode)
(add-to-list 'ac-modes 'json-mode)
(defalias 'format-json 'beautify-json)

(provide 'cb-json)
