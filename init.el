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
(setq dashboard-items '((recents . 15)
 			(bookmarks . 10)
 			(projects . 25)
 			(agenda . 15)
 			(registers . 5)))
(dashboard-setup-startup-hook)
(global-set-key (kbd "<M-f8>") 'switch-to-dashboard)

;; Change the comint mode up/down key behavior more like terminal
(require 'shell)
(define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
(define-key shell-mode-map (kbd "<down>") 'comint-next-input)
(define-key shell-mode-map (kbd "<M-up>") 'previous-line)
(define-key shell-mode-map (kbd "<M-down>") 'next-line)

;; Set Evil Mode
(when (fboundp 'evil-mode) (evil-mode 1))
(when (fboundp 'global-evil-surround-mode) (global-evil-surround-mode 1))
(define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
(evil-set-initial-state 'dashboard-mode 'emacs)

;; Set Helm
(when (fboundp 'helm-mode) (helm-mode 1))
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)

;; Set Helm-Gtags
(setq
  helm-gtags-ignore-case t
  helm-gtags-auto-update t
  helm-gtags-use-input-at-cursor t
  helm-gtags-pulse-at-cursor t
  helm-gtags-prefix-key "\C-cg"
  helm-gtags-suggested-key-mapping t
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
(bind-keys
  ("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files)
  )
; helm-gtags
(eval-after-load "helm-gtags"
  '(progn
     (evil-define-key 'normal helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
     (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "C-c r") 'helm-gtags-resume)
     ;(define-key helm-gtags-mode-map (kbd "C-=") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c g h") 'helm-gtags-show-stack)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "M-<") 'helm-gtags-next-history)
     ;(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
   )
)
; magit
(bind-keys
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup)
  )
; swith buffers (using bind-key to overwrite the keybindings in other mode)
(bind-keys*
  ("C-<tab>" . next-user-buffer)
  ("C-S-<tab>" . prev-user-buffer)
  ("C-M-<tab>" . next-buffer)
  ("C-M-S-<tab>" . previous-buffer)
  )
; misc
(bind-keys
  ("C-M-m" . toggle-frame-fullscreen)
  ("C-x 4 o" . close-and-kill-next-pane)
  ("C-x 4 k" . close-and-kill-this-pane)
  )

;; Mode hooks
; helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
; highlight sexp
(add-hook 'lisp-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)
; code folding
(add-hook 'c-mode-common-hook 'hs-minor-mode)

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

;; Use-Package
(use-package projectile
  :ensure t
  :bind ( ("C-x P" . projectile-switch-open-project)
          ("C-x p" . projectile-switch-project)
	  ("<M-f3>" . projectile-ag))
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  (use-package ag
    :ensure t))

(use-package neotree
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  :init
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action))

(use-package helm-ag
  :ensure t
  :bind ("C-M-g" . helm-ag)
  )

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

; Ref: https://github.com/kirang89/.emacs.d/blob/master/kiran/init-company.el
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind ("C-;" . company-complete)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  ;; Company Backends
  (use-package company-web
    :ensure t
    :defer t
    :bind (("C-c w" . company-web-html))
    :config
    (add-to-list 'company-backends 'company-web-html))
  (use-package company-elisp
    :bind (("C-c e" . company-elisp)))
  (use-package company-gtags
    :bind (("C-c g" . company-gtags)))
  (use-package company-yasnippet
    :config
    (add-to-list 'company-backends 'company-yasnippet))
  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers))
  (use-package company-statistics
    :ensure t
    :config
    (add-hook 'after-init-hook 'company-statistics-mode)))

; Ref: https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package company-math
  :ensure t
  :defer t)

(use-package company-quickhelp
  :ensure t
  :after company
  :bind (:map company-active-map
	      ("C-c ?" . company-quickhelp-manual-begin)))

(use-package validate
  :ensure t)

(use-package smartparens
  :ensure t
  :after validate
  :config
  (show-smartparens-global-mode 1)
  (smartparens-global-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (validate-setq
   yas-verbosity 1
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package flx-ido
  :ensure t)

(use-package p4
  :ensure t
  :config
  (p4-set-p4-config "p4config"))

(use-package dired-hacks-utils
  :ensure t
  :config
  (use-package dired-filter
    :ensure t)
  (use-package dired-subtree
    :ensure t
    :init
    (evil-define-key 'normal dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-subtree-up)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-subtree-down))
  (use-package dired-open
    :ensure t)
  (use-package dired-avfs
    :ensure t
    :if (executable-find "mountavfs"))
  (use-package dired-rainbow
    :ensure t
    :config
    (progn
	(dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
	(dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
	(dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
	(dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
	(dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
	(dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
	(dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
	(dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
	(dired-rainbow-define log "#c17d11" ("log"))
	(dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
	(dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
	(dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
	(dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
	(dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
	(dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
	(dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
	(dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
	(dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
	(dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
	(dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))
  (use-package dired-ranger
    :ensure t)
  (use-package dired-narrow
    :ensure t)
  ;; (use-package dired-list
    ;; :ensure t)
  (use-package dired-collapse
    :ensure t
    :init
    (add-hook 'dired-mode-hook (lambda () (dired-collapse-mode 1))))
)

(use-package peep-dired
  :ensure t
  :init
  (evil-define-key 'normal dired-mode-map (kbd "<SPC>") 'peep-dired)
  (evil-define-key 'normal peep-dired-mode-map (kbd "-") 'peep-dired-scroll-page-down)
  (evil-define-key 'normal peep-dired-mode-map (kbd "=") 'peep-dired-scroll-page-up)
  ;; (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  ;; (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  :config
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "mp3" "exe" "dll" "obj" "o" "pdb")))

(use-package helm-swoop
  :ensure t)

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

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
    (evil-mc ag company-c-headers jade-mode evil-indent-textobject evil-tutor evil-surround bind-key editorconfig markdown-mode helm magit smart-mode-line-powerline-theme smart-mode-line powerline monokai-theme evil dashboard helm-gtags use-package yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
