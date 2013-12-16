(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq __PROJECT-NAME__-root-path project-directory))

(add-to-list 'load-path __PROJECT-NAME__-root-path)

(require '__project-name__)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*__PROJECT-NAME__*"))
 (erase-buffer)
 (fundamental-mode)
 (deactivate-mark))

(After)
