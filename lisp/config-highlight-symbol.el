;;; config-highlight-symbol.el --- Configure highlight-symbol

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

;; Configure highlight-symbol

;;; Code:

(require 'utils-common)

(cb:install-package 'highlight-symbol t)

(custom-set-variables
 '(highlight-symbol-idle-delay 0.5))

(custom-set-faces
 '(highlight-symbol-face ((t :bold t))))

(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'sgml-mode-hook 'highlight-symbol-mode)
(add-hook 'nxml-mode-hook 'highlight-symbol-mode)

(diminish 'highlight-symbol-mode)

(provide 'config-highlight-symbol)

;;; config-highlight-symbol.el ends here
