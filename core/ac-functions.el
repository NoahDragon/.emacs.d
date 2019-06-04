;; File: ac-funcitons.el
;; Abner Chou's emacs modules
;; Customized functions.

(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(provide 'ac-functions)
