;;; cosmetic.el

(cb:require-package 'volatile-highlights)
(cb:require-package 'diminish)
(require 'hl-line)
(require 'transpose-frame)
(require 'ansi-color)

(defun cb:diminish (mode-fn &optional file)
  "Hide the given mode's lighter."
  (eval-after-load (or file (s-chop-suffix "-mode" (symbol-name mode-fn)))
    `(diminish ',mode-fn)))

(cb:diminish 'highlight-parentheses-mode)
(cb:diminish 'eldoc-mode)
(cb:diminish 'undo-tree-mode)
(cb:diminish 'projectile-mode)
(cb:diminish 'volatile-highlights-mode)
(cb:diminish 'elisp-slime-nav-mode)
(cb:diminish 'flyspell-prog-mode)
(cb:diminish 'flyspell-mode)
(cb:diminish 'yas-minor-mode "yasnippet")
(cb:diminish 'lambda-mode "lambda-mode")
(cb:diminish 'hs-minor-mode "hideshow")

;(add-hook 'prog-mode-hook 'linum-on)

(global-hl-line-mode t)
(when (display-graphic-p) (fringe-mode '(2 . 0)))
(setq-default fill-column 80)
(setq font-lock-maximum-decoration t
      color-theme-is-global        t
      ansi-color-for-comint-mode   t
      ring-bell-function           'ignore
      initial-scratch-message      nil)

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Trim whitespace on kill-line"
  (unless (bolp)
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(defun cb:hide-dos-eol ()
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M [])
  (aset buffer-display-table ?\^L []))

(add-hook 'find-file-hook 'cb:hide-dos-eol)

;;; Windows

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (numWindows (count-windows)))
      (while  (< i numWindows)
        (let* (
               (w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i numWindows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2))
               )
          (set-window-buffer w1  b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(provide 'cb-cosmetic)
