;;; hydra-window.el --- Windows operations with Hydra. -*- lexical-binding: t -*-
;; Ref: https://github.com/jojojames/matcha/blob/master/matcha-me.el

(defhydra hydra-window (:color blue :hint nil)
  "
    Window:
    Narrow/Widen            ^^Layout^^          ^^Manage^^
  ------------------------------------------------------------------------------
    _n_ Narrow Region       _._ Redo            _<f4>_ Kill Buffer and Window
    _w_ Widen               _,_ Undo            _o_ Other Window
    _ND_ Narrow to Defun    _+_ Increase Text   _l_ Delete Other Window
    _NP_ Narrow to Page     _-_ Decrease Text   _d_ Delete Window
    Frame                   ^^Window^^              ^^Resize^^
  ------------------------------------------------------------------------------
    _m_ Maximize            _=_ Balance             _<right>_ ->
    _F_ Toggle Fullscreen   _r_ Resize Windows      _<left>_ <-
    _0_ Delete Frame        _s_ Toogle WIndow Split _<down>_ Down
    _1_ Delete other Frames _t_ Rotate Windows      _<up>_ Up
    _2_ Make Frame          ___ Split Window Below
    _O_ Other Frame         _|_ Split Window Left
                          _\\_ Split Window Right
"
  ("n" narrow-to-region)
  ("w" widen)
  ("ND" narrow-to-defun)
  ("NP" narrow-to-page)
  ("." winner-redo)
  ("," winner-undo)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("<f4>" kill-buffer-and-window)
  ("l" delete-other-windows)
  ("o" other-window)
  ("d" delete-window)
  ("m" toogle-frame-maximized)
  ("F" toggle-frame-fullscreen)
  ("0" delete-frame)
  ("1" delete-other-frames)
  ("2" make-frame-command)
  ("O" other-frame)
  ("=" balance-windows)
  ("r" nil) ;j-resize-window)
  ("s" toggle-window-split)
  ("t" rotate-windows)
  ("_" split-window-below)
  ("|" split-window-left)
  ("\\" split-window-right)
  ("<right>" enlarge-window-horizontally)
  ("<left>" shrink-window-horizontally)
  ("<down>" shrink-window)
  ("<up>" enlarge-window)
)

(provide 'hydra-window)
;;; hydra-window.el ends here
