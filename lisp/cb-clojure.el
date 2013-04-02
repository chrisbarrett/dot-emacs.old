;;; cb-clojure.el

(require 'nrepl)
(require 'clojure-mode)

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
  (if-let (buf (get-buffer "*nrepl*"))
    (nrepl-switch-to-repl-buffer buf)
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

(provide 'cb-clojure)
