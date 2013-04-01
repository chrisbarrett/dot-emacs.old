;;; cb-indentation

(defvar cb:rigid-indent-modes nil
  "Lists modes that should use a rigid form of indentation.")

(defun cb:indent-rigidly? (mode)
  (and (member mode cb:rigid-indent-modes)
       (not (or (active-minibuffer-window)
                cursor-in-echo-area))))

(defadvice self-insert-command (around indent-rigidly-on-insert activate)
  (if (not (cb:indent-rigidly? major-mode))
      ad-do-it
    (indent-according-to-mode)
    ad-do-it
    (indent-according-to-mode)))

(provide 'cb-indentation)
