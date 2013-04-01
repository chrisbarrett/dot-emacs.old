;;; cb-python.el

(cb:require-package 'nose)
(cb:require-package 'python)
(cb:require-package 'pylint)
(cb:require-package 'pep8)
;; (cb:require-package 'pymacs)
;;(pymacs-load "ropemacs" "rope-")
(require 'pylookup)

(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'inferior-python-mode)

;;; Autocomplete using Ropemacs

;; (defun ac-ropemacs-candidates ()
;;   (--map (concat ac-prefix it) (rope-completions)))

;; (ac-define-source nropemacs
;;   '((candidates . ac-ropemacs-candidates)
;;     (symbol     . "p")))

;; (ac-define-source nropemacs-dot
;;   '((candidates . ac-ropemacs-candidates)
;;     (symbol     . "p")
;;     (prefix     . c-dot)
;;     (requires   . 0)))

;; (defun cb:python-ac-setup ()
;;   (--each '(ac-source-nropemacs ac-source-nropemacs-dot ac-source-yasnippet)
;;     (add-to-list 'ac-sources it)))

;; (add-hook 'inferior-python-mode-hook 'cb:python-ac-setup)
;; (add-hook 'rope-open-project-hook 'cb:python-ac-setup)
;; (add-hook 'python-mode-hook 'cb:python-ac-setup)
(define-key python-mode-map (kbd "C-c C-t") 'nosetests-all)

(provide 'cb-python)
