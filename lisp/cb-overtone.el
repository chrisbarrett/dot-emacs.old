;;; cb-overtone.el

(defvar overtone-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-g") 'cb:stop-overtone)
    km))

(define-minor-mode overtone-mode
  "Provide additional overtone-related functionality for clojure."
  nil " overtone" overtone-mode-map
  (define-key nrepl-mode-map (kbd "C-c C-g") 'cb:stop-overtone)
  ;; Jack in if there's no active connection.
  (unless (and (boundp 'nrepl-connection-list) nrepl-connection-list)
    (nrepl-jack-in)))

(defun maybe-enable-overtone-mode ()
  "Enable `overtone-mode' only if the current Clojure buffer
references overtone."
  (when (and (not overtone-mode)
             (equal major-mode 'clojure-mode)
             (string-match-p "overtone.live" (buffer-string)))
    (overtone-mode t)))

(defun cb:stop-overtone ()
  "Stop synthesis."
  (interactive)
  (nrepl-eval "(stop)")
  (message "Synthesis stopped."))

(provide 'cb-overtone)
