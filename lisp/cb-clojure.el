;;; cb-clojure.el

(cb:require-package 'clojure-mode)
(cb:require-package 'nrepl)
(cb:require-package 'ac-nrepl)
(cb:require-package 'midje-mode)

(cb:auto-mode-on-match 'clojure-mode ".cljs?$")
(cb:add-compilation-handler 'clojure-mode 'lein-uberjar 'kill-compilation)

(setq nrepl-popup-stacktraces    nil
      nrepl-hide-special-buffers t)

(add-to-list 'ac-modes 'nrepl-mode)
(set-face-attribute 'nrepl-error-highlight-face t :inherit 'error)
(set-face-underline 'nrepl-error-highlight-face nil)

(defadvice nrepl-switch-to-repl-buffer (after insert-at-end-of-nrepl-line activate)
  "Enter insertion mode at the end of the line when switching to nrepl."
  (when (and (boundp 'evil-mode)
             evil-mode
             (not (evil-insert-state-p)))
    (evil-append-line 0)))

(defadvice back-to-indentation (around move-to-nrepl-bol activate)
  "Move to position after prompt"
  (if (equal major-mode 'nrepl-mode)
      (nrepl-bol)
    ad-do-it))

(defun cb:switch-to-nrepl ()
  "Start nrepl or switch to an existing nrepl buffer."
  (interactive)
  (if (get-buffer "*nrepl*")
      (nrepl-switch-to-repl-buffer)
    (nrepl-jack-in)))

(defun cb:last-clj-buffer ()
  (--first (s-ends-with? ".clj" (buffer-name it)) (buffer-list)))

(defun cb:switch-to-last-clj-buffer ()
  "Switch to the last active clojure buffer."
  (interactive)
  (when-let (buf (cb:last-clj-buffer))
    (pop-to-buffer buf)))

(defun cb:eval-last-clj-buffer ()
  "Evaluate that last active clojure buffer without leaving the repl."
  (interactive)
  (when-let (buf (cb:last-clj-buffer))
    (with-current-buffer buf
      (nrepl-eval-buffer))))


(defun lein-uberjar ()
  (interactive)
  (compile "lein uberjar"))

;;; Hook handlers

(defun cb:on-clojure-mode ()
  ;; Activate midje-mode for unit testing.
  (midje-mode t)
  (local-set-key (kbd "C-c C-z") 'cb:switch-to-nrepl)
  (local-set-key (kbd "C-h f") 'nrepl-doc)
  (local-set-key (kbd "C-c C-f") 'nrepl-eval-buffer))

(defun cb:on-nrepl-mode ()
  (paredit-mode t)
  (ac-nrepl-setup)
  (local-set-key (kbd "C-c C-z") 'cb:switch-to-last-clj-buffer)
  (local-set-key (kbd "C-c C-f") 'cb:eval-last-clj-buffer))

(defun cb:on-nrepl-interaction-mode ()
  (paredit-mode t)
  (ac-nrepl-setup)
  (nrepl-turn-on-eldoc-mode))

(add-hook 'clojure-mode-hook 'cb:on-clojure-mode)
(add-hook 'nrepl-mode-hook 'cb:on-nrepl-mode)
(add-hook 'nrepl-interaction-mode-hook 'cb:on-nrepl-interaction-mode)

(provide 'cb-clojure)
