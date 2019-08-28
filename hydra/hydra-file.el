;;; hydra-file.el --- File operations with Hydra. -*- lexical-binding: t -*-


(defhydra hydra-file (:color blue :hint nil)
"
    File:
    _o_ Open in System Explorer
"
    ("o" browse-file-directory)
    )

(provide 'hydra-file)
;;; hydra-file.el ends here
