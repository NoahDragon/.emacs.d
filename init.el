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
(when (eq emacs-version 26.2)
  ;; fix a bug that ELPA bad request
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; idea from https://github.com/interesting-stuff/.emacs.d
(setq load-path (cons "~/.emacs.d/core"   load-path))
(require 'ac-packages)
(require 'ac-functions)
;; extra folder means something could be removed later on
(setq load-path (cons "~/.emacs.d/extra"   load-path))
(require 'highlight-sexp)
(require 'vt-mode)

;; encoding system
;; character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Set Evil Mode
;; (evil-set-initial-state 'dashboard-mode 'emacs)
;; (evil-define-key 'normal dashboard-mode-map (kbd "M-.") 'helm-gtags-dwim)

(ac-load-packages)

;; Change the comint mode up/down key behavior more like terminal
(require 'shell)
(define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
(define-key shell-mode-map (kbd "<down>") 'comint-next-input)
(define-key shell-mode-map (kbd "<M-up>") 'previous-line)
(define-key shell-mode-map (kbd "<M-down>") 'next-line)

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
(if (display-graphic-p)
  (progn
    ;; Display lambda as λ
    (when (fboundp global-prettify-symbols-mode) (global-prettify-symbols-mode 1))
    ;; Show line number
    (when (fboundp 'global-nlinum-mode) (global-nlinum-mode t))
    )
  )


;; Set keys binding
; swith buffers (using bind-key to overwrite the keybindings in other mode)
(bind-keys*
  ("C-<tab>" . next-user-buffer)
  ("C-S-<tab>" . prev-user-buffer)
  ("C-M-<tab>" . next-buffer)
  ("C-M-S-<tab>" . previous-buffer)
  )
; Misc
(bind-keys
  ("C-M-m" . toggle-frame-fullscreen)
  )

;; Hooks
; highlight sexp
(add-hook 'lisp-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
; code folding
(add-hook 'c-mode-common-hook 'hs-minor-mode)
; Remove trailing whitespaces when save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Dired setting
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
; allow dired to delete or copy dir
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once
(setq dired-dwim-target t) ; open other dired window and use it as target
(require 'dired )
; make dired use the smae buffer for viewing directory
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; Indention settings
; Disable Tab indent
;; (setq-default indent-tabs-mode nil)
; Disable the new line auto indent
; Cause the electric indent mode has disable, so bind enter to enable newline indent
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(global-set-key (kbd "RET") 'newline-and-indent)
;; Show column numbers in mode line.
(setq column-number-mode t)
;; Never ring the bell. Never.
(setq ring-bell-function (lambda()))
;; Don't disable any commands (e.g. `upcase-region').
(setq disabled-command-function nil)
;; Don't use dialog boxes.
(setq use-dialog-box nil)


;; Completion ignores filenames ending in any string in this list.
(setq completion-ignored-extensions
      '(".o" ".elc" "~" ".bin" ".class" ".exe" ".ps" ".abs" ".mx"
        ".~jv" ".rbc" ".pyc" ".beam" ".aux" ".out" ".pdf" ".hbc"
        ".obj" ".map"))


;; System Specific
(if (eq system-type 'darwin)
    (progn
    ;; Fix the logo display issue on Mac
    ;; https://emacs.stackexchange.com/questions/20976/x11-why-is-the-emacs-logo-image-missing-on-the-welcome-screen
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
      ;; The original value added to the 'image-height' for the test was 19; however,
      ;; that causes the test to fail on X11 by about 1.5 -- so use 17 instead.
      (> frame-height (+ image-height 17)))))))
        (setq ispell-program-name "/usr/local/bin/aspell")
        ;; Use ripgrep as difault search engine
        (use-package helm-rg
          :ensure t
          :bind ("C-M-g" . helm-rg))
        (use-package projectile-ripgrep
          :ensure t
          :bind ("<M-f3>" . projectile-ripgrep))
        ; To enable flyspell, the backend aspell should be installed.
        (use-package flyspell-correct-helm
      :ensure t)
  )
)

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
    (company-anaconda evil-nerd-commenter evil-mc ag company-c-headers jade-mode evil-indent-textobject evil-tutor evil-surround bind-key editorconfig markdown-mode helm magit smart-mode-line-powerline-theme smart-mode-line powerline monokai-theme evil dashboard helm-gtags use-package yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
