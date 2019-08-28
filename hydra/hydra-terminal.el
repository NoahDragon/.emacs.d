;;; hydra-terminal.el --- Terminal options with Hydra. -*- lexical-binding: t -*-

(defhydra hydra-terminal (:color blue :hint nil)
"
    Terminal:
    _s_ shell/cmd
    _e_ eshell
    _t_ terminal
"
    ("s" shell)
    ("e" eshell)
    ("t" term)
)

(provide 'hydra-terminal)
;;; hydra-terminal.el ends here
