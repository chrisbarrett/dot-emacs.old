;;; config-calc.el --- Configuration for calc.

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
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

;; Configuration for calc.

;;; Code:

(require 'utils-common)
(require 'utils-ui)

(defun calc-dwim ()
  "Run calc or grab the current region."
  (interactive)
  (if (region-active-p)
      (condition-case err
          (let ((opt (read-option
                      "Calc Grab" 'car 'cadr
                      '(("v" "Grab as Vector" calc-grab-region)
                        ("m" "Grab as Matrix" calc-grab-rectangle)
                        ("c" "Sum Cols" calc-grab-sum-down)
                        ("r" "Sum Rows" calc-grab-sum-across)))))
            (call-interactively (nth 2 opt)))

        (error
         (message "Malformed region. %s" (error-message-string err))))

    (call-interactively 'calc)))

(bind-key* "<f2>" 'calc-dwim)
(bind-key* "C-/"  'quick-calc)

(after 'calc-units
  (setq math-additional-units
        '((bit nil "*Bit")
          (B   "8 * bit" "Byte")
          (KiB "1024 * B"   "Kebibyte")
          (MiB "1024 * KiB" "Mebibyte")
          (GiB "1024 * MiB" "Gibibyte")
          (TiB "1024 * GiB" "Tebibyte")
          (PiB "1024 * TiB" "Pebibyte")
          (EiB "1024 * PiB" "Exbibyte")
          (ZiB "1024 * EiB" "Zebibyte")
          (YiB "1024 * ZiB" "Yobibyte")
          )
        math-units-table nil))


(provide 'config-calc)

;;; config-calc.el ends here
