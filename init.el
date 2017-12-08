;; init.el --- Where all them magic begins
;;
;; Get rid of the .emacs to be more version control friendly.
;; All settings should goes here, or sub files.
;; 
(require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
  (when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))

;; initialise the package system.
(package-initialize)

;; idea from https://github.com/interesting-stuff/.emacs.d
(setq load-path (cons "~/.emacs.d/core"   load-path))
(require 'ac-packages)

;; encoding system
;; character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; theme
; load theme
(load-theme 'monokai t)
; smart mode line for status bar
(setq custom-safe-themes t)
; (setq sml/no-confirm-load-theme t) ; this line will create init.elc file
(setq sml/theme 'powerline)
(sml/setup)

;; set up the dashboard for welcome
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '((recents . 5)
 			(bookmarks . 5)
 			(projects . 5)
 			(agenda . 5)
 			(registers . 5)))
(dashboard-setup-startup-hook)

;; Set Evil Mode
(when (fboundp 'evil-mode) (evil-mode 1))
(when (fboundp 'global-evil-surround-mode) (global-evil-surround-mode 1))

;; Set Helm
(when (fboundp 'helm-mode) (helm-mode 1))

;; Company Mode
(add-hook 'after-init-hook 'global-company-mode)

;; Turn on editorconfig
(setq editorconfig-get-properties-function
           'editorconfig-core-get-properties-hash)
(when (fboundp 'editorconfig-mode) (editorconfig-mode 1))

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

;; Set backup files into temp folder
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
; Deleting the backup files if it is older than a week
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; GUI display setting
; Hide welcome page
(setq inhibit-startup-screen t)
; Maximize the windows on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
; Disable menubar, toolbar, scrollbar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar -1))
; Disable line wrapping
(when (fboundp 'toggle-truncate-lines) (toggle-truncate-lines -1))
; Display y-n instead of yes-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set keys binding
; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
; swith buffers (using bind-key to overwrite the keybindings in other mode)
(bind-keys*
  ("C-<tab>" . switch-to-next-buffer)
  ("C-S-<tab>" . switch-to-prev-buffer)
  )

;; Indention settings
; Disable the new line auto indent
; Cause the electric indent mode has disable, so bind enter to enable newline indent
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(global-set-key (kbd "RET") 'newline-and-indent)

;; Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    (quote
      ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(package-selected-packages
    (quote
      (jade-mode evil-indent-textobject evil-tutor evil-surround bind-key editorconfig company markdown-mode helm magit smart-mode-line-powerline-theme smart-mode-line projectile powerline monokai-theme evil dashboard))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
