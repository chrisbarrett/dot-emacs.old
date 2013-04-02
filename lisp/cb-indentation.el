;;; cb-indentation

(define-minor-mode rigid-indentation-mode
  "Minor mode where re-indentation is applied before and after
  each editing action."
  nil nil nil)

(defadvice self-insert-command (around indent-rigidly-on-insert activate)
  (if (and (boundp 'rigid-indent-mode)
           rigid-indent-mode
           (not (or (active-minibuffer-window) cursor-in-echo-area)))
      ad-do-it
    (indent-according-to-mode)
    ad-do-it
    (indent-according-to-mode)))

(provide 'cb-indentation)
