;; Author: Abner Chou
;; Date: 2019 Jun 27
;; Syntax highlights for VT file
;; Ref: http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;;      https://www.emacswiki.org/emacs/ModeTutorial

(defvar vt-mode-hook nil)

(defvar vt-mode-syntax-table
  (let ((st (make-syntax-table)))
	(modify-syntax-entry ?_ "w" st)
	(modify-syntax-entry ?/ ". 123b" st)
	(modify-syntax-entry ?* ". 23" st)
	(modify-syntax-entry ?\n "> b" st)
	st)
  "Syntax table for vt-mode")
    

(setq vt-font-lock-keywords
      (let*(
	      (x-keywords '("VTConfiguration"))
	      (x-constants '("Enable" "Disable"))

	      (x-constants-regexp (regexp-opt x-keywords 'words))
	      (x-keywords-regexp (regexp-opt x-keywords 'words))
	      )
	   `(
	     (,x-constants-regexp . font-lock-constant-face)
	     (,x-keywords-regexp . font-lock-keyword-face)
	     ;; note: order above matters, because once colored, that part won't change.
	     ;; in general, put longer words first
	     )
	   )
      )

;;;###autoload
(define-derived-mode vt-mode lisp-mode "VT mode"
  "Major mode for editing VT files, providing the basic sytax highlights."
  ;; code for syntax highlighting
  (set-syntax-table vt-mode-syntax-table)
  (setq font-lock-defaults '((vt-font-lock-keywords)))
  )
(add-to-list 'auto-mode-alist '("\\.vt\\'" . vt-mode))


(provide 'vt-mode)
