;;; hydra-file.el --- File operations with Hydra. -*- lexical-binding: t -*-

(defhydra hydra-search (:color blue :hint nil)
"
    Search:
    _s_ rg dwim
"
    ("s" rg-dwim-current-file)
)

(provide 'hydra-search)
;;; hydra-search.el ends here
