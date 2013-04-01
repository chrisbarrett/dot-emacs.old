;;; cb-auto-complete

(cb:require-package 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(global-auto-complete-mode t)

(ac-flyspell-workaround)
(ac-linum-workaround)

(add-to-list 'ac-modes 'wisent-grammar-mode)

(add-to-list 'ac-dictionary-directories (concat user-etc-dir "ac-dict/"))

(setq ac-comphist-file (concat user-tmp-dir "ac-comphist.dat")
      ac-auto-show-menu t
      ;; Show popup even if a word is uniquely completed.
      ac-dwim nil
      ac-use-menu-map t
      ac-use-quick-help t
      ac-quick-help-delay 0.2
      ac-quick-help-height 60)

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))

;;; Keyboard bindings

(defun cb:ac-complete-space ()
  "Autocomplete then insert a space."
  (interactive)
  (ac-complete)
  (just-one-space))

(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map (kbd "TAB")   'ac-complete)
(define-key ac-completing-map (kbd "ESC")   'ac-stop)
(define-key ac-completing-map (kbd "RET")   'cb:ac-complete-space)




(provide 'cb-auto-complete)
