;;; hydra-space.el --- Shortcuts hub with Hydra. -*- lexical-binding: t -*-
(require 'hydra-p4)
(require 'hydra-window)
(require 'hydra-hs)
(require 'hydra-file)
(require 'hydra-search)
(require 'hydra-terminal)
(defhydra hydra-space (:exit t)
    "Space Shortcuts"
    ("b" helm-buffers-list "list-buffers")
    ("d" switch-to-dashboard "dashboard")
    ("o" hydra-file/body "file operations")
    ("r" revert-buffer "revert")
    ("s" hydra-search/body "search")
    ("t" hydra-terminal/body "terminal")
    ("w" hydra-window/body "window")
    (":" eval-expression "eval expression")
    ("hs" hydra-hs/body "code folding")
    ("p4" hydra-p4/body "p4 version control")
    ("SPC" evil-avy-goto-word-0 "goto-word")
)

(provide 'hydra-space)
;;; hydra-space.el ends here
