;;; cb-ido
;;;
;;; ido configuration.

(cb:require-package 'ido)
(cb:require-package 'ido-ubiquitous)
(cb:require-package 'idomenu)

(add-to-list 'ido-ignore-buffers "*helm mini*")

(ido-mode +1)
(ido-ubiquitous-mode +1)
(icomplete-mode +1)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)
