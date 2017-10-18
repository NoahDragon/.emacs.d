;; init.el --- Where all them magic begins
;;
;; Get rid of the .emacs to be more version control friendly.
;; All settings should goes here, or sub files.
;; 

;; Set MELPA source
(require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
  (when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))

;; Theme
(load-theme 'monokai t)

;; Set up the dashboard for welcome
(require 'dashboard)
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '((recents . 5)
 			(bookmarks . 5)
 			(projects . 5)
 			(agenda . 5)
 			(registers . 5)))
(dashboard-setup-startup-hook)

;; Set Evil Mode
(require 'evil)
(evil-mode 1)

;; Set WindMove using shift+arrow keys to switch between windows
;; Build in above version 21
(if (version< emacs-version "24.1")
    (); Do nothing
  (windmove-default-keybindings))

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

;; Disable menubar, toolbar, scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Set keys binding
; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-x") 'smex-major-mode-commands)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
