;; Author: Abner Chou
;; Date: 2019 Jun 27
;; Syntax highlights for VT file
;; Ref: http://ergoemacs.org/emacs/elisp_syntax_coloring.html
;;      https://www.emacswiki.org/emacs/ModeTutorial

(defvar vt-mode-hook nil)

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
  (setq font-lock-defaults '((vt-font-lock-keywords)))
  (modify-syntax-entry ?_ "w" vt-mode-syntax-table)
  (modify-syntax-entry ?/ ". 123b" vt-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" vt-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" vt-mode-syntax-table)
  )
(add-to-list 'auto-mode-alist '("\\.vt\\'" . vt-mode))


(provide 'vt-mode)
