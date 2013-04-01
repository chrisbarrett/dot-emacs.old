;;; cb-evil

(cb:require-package 'evil)
(cb:require-package 'surround)
(cb:require-package 'evil-numbers)

(evil-mode +1)
(global-surround-mode +1)

(setq evil-want-visual-char-semi-exclusive t
      evil-toggle-key (kbd "M-z")
      evil-default-cursor t)

(setq-default evil-shift-width 2)

(defun cb:evil-undefine ()
  "Temporarily undefine a key for Evil minor mode."
  (interactive)
  (let ((evil-mode-map-alist))
    (call-interactively (key-binding (this-command-keys)))))

(define-key evil-normal-state-map (kbd "C-z") 'cb:evil-undefine)
(define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/dec-at-pt)
(define-key evil-insert-state-map (kbd "C-z") 'cb:evil-undefine)
(define-key evil-visual-state-map (kbd "C-z") 'cb:evil-undefine)

;; Use ESC as quit command in most situations.
(--each '(evil-normal-state-map
          evil-visual-state-map
          minibuffer-local-map
          minibuffer-local-ns-map
          minibuffer-local-completion-map
          minibuffer-local-must-match-map
          minibuffer-local-isearch-map)
  (define-key (eval it) [escape] 'keyboard-quit))

(provide 'cb-evil)
