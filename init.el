;; init.el --- Where all them magic begins
;;
;; Get rid of the .emacs to be more version control friendly.
;; All settings should goes here, or sub files.
;;

;; Global constants
(defconst IS-WIN   (memq system-type '(windows-nt ms-dos)))
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

;; Set up garbage collection and tweak startup settings.
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 402653184
                   gc-cons-percentage 0.6)
             (add-hook
              'focus-out-hook
              (lambda ()
                "Lower `gc-cons-threshold' and then run `garbage-collect'."
                (let ((gc-cons-threshold 800000))
                  (garbage-collect))))) t)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; In Emacs 27, this is handled by `early-init'.
(when (< emacs-major-version 27)
  (load "~/.emacs.d/early-init.el")
  (require 'early-init))

(setq package-check-signature nil)
;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require 'ac-packages)
(require 'ac-functions)
(ac-load-packages)
(require 'highlight-sexp)
(require 'vt-mode)

;; encoding system
;; character encodings default to utf-8.
(when (fboundp 'set-charset-priority) (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq inhibit-compacting-font-caches t)

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
               (> (- current (float-time (cl-fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; GUI display setting
; Hide welcome page
(setq inhibit-startup-screen t)
; Maximize the windows on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
; GUI Tweats
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'toggle-scroll-bar) (toggle-scroll-bar -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
; Make things a little more responsive in general
(setq echo-keystrokes 0.1
      tooltip-delay 0
      tooltip-short-delay 0)
; Disable line wrapping
(when (fboundp 'toggle-truncate-lines) (toggle-truncate-lines -1))
; Display y-n instead of yes-no
(defalias 'yes-or-no-p 'y-or-n-p)
; Kill process without confirmation
(setq confirm-kill-processes nil)
; Highlight the current line to make the cursor easier to see
(global-hl-line-mode)
; Ensure the help windows is elected when it is open.
(setq help-window-select t)

(if (display-graphic-p)
  (progn
    ;; Display lambda as λ
    (when (fboundp 'global-prettify-symbols-mode) (global-prettify-symbols-mode 1))
    )
  )

;; Set keys binding
; swith buffers (using bind-key to overwrite the keybindings in other mode)
(bind-keys*
  ("C-<tab>" . next-user-buffer)
  ("C-S-<tab>" . prev-user-buffer)
  ("C-M-<tab>" . next-buffer)
  ("C-M-S-<tab>" . previous-buffer)
  ("C-<f4>" . kill-buffer-and-window)
  )
(windmove-default-keybindings)
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
; Mac
(if IS-MAC
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
  )
)
; Windows
(if IS-WIN
  (progn
    ;; Fix the issue that system PATH not synced
    (add-to-list 'exec-path (expand-file-name "~/.emacs.d/thirdparties/win/hunspell-1.3.2-3-w32"))
    (add-to-list 'exec-path (expand-file-name "~/.emacs.d/thirdparties/win/es-1.1.0.15"))
    (add-to-list 'exec-path (expand-file-name "~/.emacs.d/thirdparties/win/clang_lib_8.0.0"))
    (add-to-list 'exec-path (expand-file-name "~/scoop/shims"))
    (setenv "PATH" (mapconcat #'identity exec-path path-separator))
    )
)

(server-start)

;; Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(flycheck-checker-error-threshold nil)
 '(package-selected-packages
   (quote
    (helm-projectile projectile-ripgrep company-anaconda evil-nerd-commenter evil-mc ag company-c-headers jade-mode evil-indent-textobject evil-tutor evil-surround bind-key editorconfig markdown-mode helm magit smart-mode-line-powerline-theme smart-mode-line powerline monokai-theme evil dashboard helm-gtags use-package yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
