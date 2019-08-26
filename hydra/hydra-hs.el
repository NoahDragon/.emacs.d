;;; hydra-hs.el --- Code folding with Hydra. -*- lexical-binding: t -*-
;; From: https://github.com/abo-abo/hydra/wiki/Emacs

(defhydra hydra-hs (:color blue :hint nil)
   "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level

_SPC_ cancel
"
   ("s" hs-show-all)
   ("h" hs-hide-all)
   ("a" hs-show-block)
   ("d" hs-hide-block)
   ("t" hs-toggle-hiding)
   ("l" hs-hide-level)
   ("n" forward-line)
   ("p" (forward-line -1))
   ("SPC" nil)
)

(provide 'hydra-hs)
;;; hydra-hs.el ends here
