;;; hydra-window.el --- Windows operations with Hydra. -*- lexical-binding: t -*-
;; Ref: https://github.com/jojojames/matcha/blob/master/matcha-me.el

(defhydra hydra-window (:color blue :hint nil)
  "
    Window:
    Narrow/Widen            ^^Layout^^          ^^Text^^
  ------------------------------------------------------------------------------
    _n_ Narrow Region       _._ Redo           _+_ Increase
    _w_ Widen               _,_ undo           _-_ Decrease
    _ND_ Narrow to Defun
    _NP_ Narrow to Page
    Frame                   ^^Window^^              ^^Resize^^
  ------------------------------------------------------------------------------
    _m_ Maximize            _=_ Balance             _<right>_ ->
    _F_ Toggle Fullscreen   _r_ Resize Windows      _<left>_ <-
    _0_ Delete Frame        _s_ Toogle WIndow Split _<down>_ Down
    _1_ Delete other Frames _t_ Rotate Windows      _<up>_ Up
    _2_ Make Frame          _-_ Split Window Below
    _o_ Other Frame         _|_ Split Window Left
                          _\\_ Split Window Right
"
  ("n" narrow-to-region)
  ("w" widen)
  ("ND" narrow-to-defun)
  ("NP" narrow-to-page)
  ("." winner-redo)
  ("," winner-undo)
  ("+" text-scale-increase)
  ("-" text-scale-descrease)
  ("m" toogle-frame-maximized)
  ("F" toggle-frame-fullscreen)
  ("0" delete-frame)
  ("1" delete-other-frames)
  ("2" make-frame-command)
  ("o" other-frame)
  ("=" balance-windows)
  ("r" nil) ;j-resize-window)
  ("s" toggle-window-split)
  ("t" rotate-windows)
  ("_" split-window-below)
  ("|" split-window-left)
  ("\\" split-window-right)
  ("<right>" shrink-window-horizontally)
  ("<left>" enlarge-window-horizontally)
  ("<down>" shrink-window)
  ("<up>" enlarge-window)
)

(provide 'hydra-window)
;;; hydra-window.el ends here
