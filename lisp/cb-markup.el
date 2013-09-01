;;; cb-markup.el --- Configuration for markup languages

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20130527.0006

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

;; Configuration for markup languages

;;; Code:

(require 'use-package)
(require 'cb-lib)

(after 'smart-operator
  (hook-fn 'cb:markup-modes-hook
    (local-set-key (kbd ",") (smart-op ","))))

(after 'smartparens
  (sp-with-modes '(sgml-mode html-mode)
    (sp-local-tag  "<" "<_>" "</_>"
                   :transform 'sp-match-sgml-tags)))

(defun cb:xml-one-liner? (str)
  (save-match-data
    (-when-let (match (->> str (s-match
                                (rx bol (* (any space "\t"))
                                    "<" (group (+ word)) (* nonl) ">"
                                    (group (+ nonl))
                                    "</" (group (* word)) ">"
                                    (* (any space "\t")) eol))))
      (and (equal (nth 1 match)
                  (nth 3 match))
           (not (s-contains? "<" (nth 2 match)))))))

(defun cb:pp-xml (xml)
  "Return a reformatted version of an XML string.
Puts each XML node on a separate line, except for one-liners."
  (with-temp-buffer
    (insert xml)
    (nxml-mode)
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx (not (any "%")) ">"
                (group-n 1 (* (any space "	")))
                "<" (not (any "%")))
            nil t)
      (unless (cb:xml-one-liner? (thing-at-point 'line))
        (replace-match "\n" nil nil nil 1)))
    (buffer-string)))

(defun cb:reformat-xml ()
  "Insert newlines and indent XML.
  Operates on region, or the whole buffer if no region is defined."
  (interactive)
  (let* ((line (line-number-at-pos))
         (col  (current-column))
         (reg (if (region-active-p)
                  (list (region-beginning) (region-end))
                (list (point-min) (point-max))))
         (str (apply 'buffer-substring reg)))
    (atomic-change-group
      (apply 'delete-region reg)
      (insert (cb:pp-xml str))
      (apply 'indent-region reg))
    ;; Return to original position.
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)))

(use-package nxml-mode
  :commands nxml-mode
  :mode (("\\.xml" . nxml-mode)
         ("\\.plist" . nxml-mode))
  :init
  (progn
    (hook-fn 'nxml-mode-hook
      (local-set-key (kbd "M-q") 'cb:reformat-xml))

    (hook-fn 'find-file-hook
      "Enable nxml-mode if when visiting a file with a DTD."
      (when (s-starts-with? "<?xml " (buffer-string))
        (nxml-mode)))))

(use-package sgml-mode
  :defer t
  :init
  (hook-fn 'sgml-mode-hook
    (setq-default sgml-xml-mode t)
    (local-set-key (kbd "M-q") 'cb:reformat-xml)))

(use-package html-mode
  :defer t
  :init
  (after 'auto-complete
    (defconst cb:html-tag-attrs
      '("class" "id")
      "Global attributes that appear in HTML tags.")

    (ac-define-source html-tag-source
      '((candidates . (-when-let (tag (te/current-tag))
                        (and (te/eligible-for-auto-attribute-insert?)
                             (not (s-starts-with? "%" (te/get tag :name)))
                             (--none? (te/has-attribute it tag)
                                      cb:html-tag-attrs)

                             cb:html-tag-attrs)))
        (symbol     . "a")
        (requires   . '(tagedit))
        (action     . (lambda ()
                        (insert "=\"\"")
                        (unless (thing-at-point-looking-at ">")
                          (just-one-space))
                        (search-backward "=")
                        (forward-char 2)))))

    (add-to-list 'ac-modes 'html-mode)
    (hook-fn 'html-mode-hook
      (auto-complete-mode +1)
      (add-to-list 'ac-sources 'ac-source-html-tag-source))))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"          . gfm-mode)
         ("\\.[mM]arkdown$" . gfm-mode))
  :config
  (progn

    (after 'smartparens
      (sp-with-modes '(markdown-mode)
        (sp-local-pair "```" "```")))

    (evil-define-keys 'normal markdown-mode-map
      "M-P" 'outline-previous-visible-heading
      "M-N" 'outline-next-visible-heading)

    ;; Customise faces.
    (set-face-font markdown-inline-code-face (monospace-font))
    (set-face-font markdown-pre-face (monospace-font))
    (set-face-font markdown-url-face (monospace-font))
    (set-face-font markdown-header-delimiter-face (monospace-font))
    (set-face-font markdown-header-rule-face (monospace-font))
    (set-face-font markdown-list-face (monospace-font))
    (set-face-attribute markdown-header-face-1 nil :height 200)
    (set-face-attribute markdown-header-face-2 nil :height 160)
    (set-face-font markdown-header-face (sans-serif-font))

    (hook-fn 'markdown-mode-hook
      (buffer-face-set `(:family ,(serif-font) :height 130))
      (setq imenu-generic-expression
            '(("title"  "^\\(.*\\)[\n]=+$" 1)
              ("h2-"    "^\\(.*\\)[\n]-+$" 1)
              ("h1"   "^# \\(.*\\)$" 1)
              ("h2"   "^## \\(.*\\)$" 1)
              ("h3"   "^### \\(.*\\)$" 1)
              ("h4"   "^#### \\(.*\\)$" 1)
              ("h5"   "^##### \\(.*\\)$" 1)
              ("h6"   "^###### \\(.*\\)$" 1)
              ("fn"   "^\\[\\^\\(.*\\)\\]" 1))))))

(use-package creole-mode
  :commands creole-mode
  :ensure t
  :config
  (progn

    (defun creole-insert-link (url desc)
      (interactive "sURL: \nsDescription: ")
      (insert (format "[[%s|%s]]" url desc)))

    (define-key creole-mode-map (kbd "C-c C-l") 'creole-insert-link)))

(provide 'cb-markup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; cb-markup.el ends here
