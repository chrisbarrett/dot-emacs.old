;;; cb-autopair

(cb:require-package 'autopair)
(setq autopair-autowrap t)

(defadvice autopair-mode (around cb:maybe-enable-autopair activate)
  "Do not enable autopair if paredit is active."
  (unless (and (boundp 'paredit-mode) paredit-mode)
    ad-do-it))

(autopair-global-mode t)

(provide 'cb-autopair)
