;; init.el --- Where all them magic begins
;;
;; Get rid of the .emacs to be more version control friendly.
;; All settings should goes here, or sub files.
;; 

;; Set MELPA source
(require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/pakcages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))

;; Set Evil Mode
(require 'evil)
  (evil-mode 1)

;; Set Font that support Chinese character on Windows
(if (eq system-type 'windows-nt)
    (add-to-list 'default-frame-alist '(font . "YaHei Consolas Hybrid"))
)

;; Fix the logo display issue on Mac
;; https://emacs.stackexchange.com/questions/20976/x11-why-is-the-emacs-logo-image-missing-on-the-welcome-screen
(if (eq system-type 'darwin)
  (defun use-fancy-splash-screens-p ()
    "Return t if fancy splash screens should be used."
    (when (and (display-graphic-p)
               (or (and (display-color-p)
            (image-type-available-p 'xpm))
                   (image-type-available-p 'pbm)))
      (let ((frame (fancy-splash-frame)))
        (when frame
    (let* ((img (create-image (fancy-splash-image-file)))
           (image-height (and img (cdr (image-size img nil frame))))
           ;; We test frame-height so that, if the frame is split
           ;; by displaying a warning, that doesn't cause the normal
           ;; splash screen to be used.
           (frame-height (1- (frame-height frame))))
     ;; The original value added to the `image-height' for the test was 19; however,
     ;; that causes the test to fail on X11 by about 1.5 -- so use 17 instead.
     (> frame-height (+ image-height 17)))))))
)
