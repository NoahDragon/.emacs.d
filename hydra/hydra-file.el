;;; hydra-file.el --- File operations with Hydra. -*- lexical-binding: t -*-

(defhydra hydra-file (:color blue :hint nil)
"
    File:
    File                           ^^Content^^
  --------------------------------------------------------------------------
    _o_ Open in System Explorer    _h_ Replace
    _r_ Rename Rurrent File        _j_ Replace Regexp
"
    ("o" browse-file-directory)
    ("r" rename-current-buffer-file)
    ("h" replace-string)
    ("j" replace-regexp)
    )

(provide 'hydra-file)
;;; hydra-file.el ends here
