;;; cb-colour.el

(require 'color-theme)

(use-package color-theme-solarized)
(use-package ir-black-theme)

(defface paren-face
  '((((class color) (background dark))
     (:foreground "grey30"))
    (((class color) (background light))
     (:foreground "grey80")))
  "Face used to dim parentheses."
  :group 'lisp)

(defun solarized-light ()
  (interactive)
  (load-theme 'solarized-light 'no-confirm)
  (set-face-background  'helm-selection "white")
  (set-face-underline   'helm-selection nil)
  (set-face-foreground  'show-paren-match-face "black")
  (set-face-bold        'show-paren-match-face t)
  (when (display-graphic-p) (set-face-foreground  'paren-face "grey80"))
  (set-face-background  'error "LightPink")
  (set-face-foreground  'error "black")
  (set-face-background  'warning "LightBlue")
  (set-face-foreground  'warning "black"))

(defun solarized-dark ()
  (interactive)
  (load-theme 'solarized-dark 'no-confirm)
  (set-face-background  'helm-selection "black")
  (set-face-underline   'helm-selection nil)
  (set-face-foreground  'show-paren-match-face "white")
  (set-face-foreground  'paren-face (if (display-graphic-p) "grey30" "blue"))
  (set-face-bold        'show-paren-match-face t)
  (set-face-background  'show-paren-match-face nil)
  (set-face-background  'error "Firebrick4")
  (set-face-foreground  'error "gray40")
  (set-face-background  'warning "DarkBlue")
  (set-face-foreground  'error "gray80"))

(defun ir-black ()
  (interactive)
  (require 'ir-black-theme)
  (load-theme 'ir-black 'no-confirm)
  (set-face-foreground  'font-lock-doc-face "DarkOliveGreen4")
  (set-face-italic      'font-lock-doc-string-face t)
  (set-face-background  'linum "gray15")
  (set-face-foreground  'default "grey50")
  (set-face-foreground  'paren-face "grey20")
  (set-face-foreground  'show-paren-match-face "green")
  (set-face-bold        'show-paren-match-face t)
  (set-face-background  'show-paren-match-face nil)
  (set-face-underline   'hl-line nil))


;;; Set colour by time of day.
(let ((hour (string-to-number (substring (current-time-string) 11 13))))
  (cond
   ((and (display-graphic-p) (<= 0 hour) (>= 6 hour))
    (ir-black))

   ((or (< 20 hour) (> 9 hour))
    (solarized-dark))

   (t
    (solarized-light))))

(provide 'cb-colour)
