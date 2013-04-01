;;; cb-key-chord.el

(cb:require-package 'key-chord)

(key-chord-mode +1)

;; Global keys
(key-chord-define-global "dh" 'helm-mini)
(key-chord-define-global "x;" 'cb:kill-current-buffer)
(key-chord-define-global "fh" 'idomenu)

(eval-after-load 'paredit
  '(progn
     (key-chord-define paredit-mode-map "qj" 'paredit-backward-slurp-sexp)
     (key-chord-define paredit-mode-map "qk" 'cb:paredit-forward-slurp-sexp-neatly)
     (key-chord-define paredit-mode-map "ql" 'paredit-splice-sexp-killing-backward)
     (key-chord-define paredit-mode-map "qn" 'paredit-backward-barf-sexp)
     (key-chord-define paredit-mode-map "qm" 'paredit-forward-barf-sexp)))

(provide 'cb-key-chord)
