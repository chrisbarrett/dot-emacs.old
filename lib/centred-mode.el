;;; centred-mode.el --- Provides a centred view of the buffer text for easier reading.

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((cl-lib "0.3"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides a centred view of the buffer text for easier reading.
;;
;; Install using `package-install-file', then bind to a key.

;;; Code:

(require 'cl-lib)

(defgroup centred-mode nil
  "Provides a centred view of the buffer text for easier reading."
  :group 'productivity
  :prefix "centred-mode")

(defcustom centred-mode-before-change-margins-hook nil
  "Hook run before the margins are modified when activating `centred-mode'."
  :group 'centred-mode
  :type 'hook)

(defcustom centred-mode-toggled-hook nil
  "Hook run after toggling `centred-mode' on or off."
  :group 'centred-mode
  :type 'hook)

;;; Internal

(defvar-local centred-mode--default-margins nil
  "The state of the margins before activating `centred-mode'.
Its value is used to restore the previous margin state when
deactivating the mode.")

(defun centred-mode--calculate-margins ()
  "Return the margin sizes to use to center text in the buffer."
  (let* ((center fill-column)
         (margin  (max 0 (floor (/ (- (frame-width) center) 2.0)))))
    (list margin margin)))

(defun centred-mode--apply-default-margins ()
  "Restore margins to their state before `centred-mode' was enabled."
  (apply 'set-window-margins nil centred-mode--default-margins))

(defun centred-mode--apply-centering (&optional force)
  "Apply appropriate centering style for the current window state.

The optional argument FORCE is used during mode initialisation to
override the default checks."
  (cond
   ((/= 1 (length (window-list)))
    (centred-mode--apply-default-margins))

   ((or (and (boundp 'centred-mode) centred-mode)
        force)
    (apply 'set-window-margins nil (centred-mode--calculate-margins)))))

;;;###autoload
(define-minor-mode centred-mode
  "Minor mode that expands the fringes of the buffer to create a
centred view for reading.

The text is restored to its previous state if more than one
window is opened."
  :init-value nil
  :lighter " Centred"
  (cond
   (centred-mode
    (run-hooks 'centred-mode-before-change-margins-hook)

    (cl-destructuring-bind (l . r) (window-margins)
      (setq-local centred-mode--default-margins (list l r)))

    (add-hook 'window-configuration-change-hook
              'centred-mode--apply-centering nil t)

    (centred-mode--apply-centering t))

   (t
    (remove-hook 'window-configuration-change-hook
                 'centred-mode--apply-centering t)
    (centred-mode--apply-default-margins))))

;;;###autoload
(defun toggle-centred-mode ()
  "Toggle whether `centred-mode' is enabled for the current buffer."
  (interactive)
  (centred-mode (if centred-mode -1 +1))
  (run-hooks 'centred-mode-toggled-hook))

;;; Linum compatibility.

(defvar-local centred-mode--default-linum nil)

(defun centred-mode--restore-linum ()
  "Restore linum state after disabling `centred-mode'."
  (when (and (boundp 'linum-mode) centred-mode--default-linum)
    (linum-mode +1)))

(defun centred-mode--save-linum ()
  "Save linum state and disable for `centred-mode'."
  (when (boundp 'linum-mode)
    (setq-local centred-mode--default-linum linum-mode)
    (when linum-mode
      (linum-mode -1))))

;;;###autoload
(add-hook 'centred-mode-before-change-margins-hook 'centred-mode--save-linum)
;;;###autoload
(add-hook 'centred-mode-off-hook 'centred-mode--restore-linum)

;;; w3m compatibility

(defun centred-mode--redisplay-w3m ()
  "Refresh the current w3m buffer so text is laid out."
  (when (and (fboundp 'w3m-redisplay-this-page)
             (derived-mode-p 'w3m-mode))
    (w3m-redisplay-this-page)))

(add-hook 'centred-mode-toggled-hook 'centred-mode--redisplay-w3m)


(provide 'centred-mode)

;;; centred-mode.el ends here
