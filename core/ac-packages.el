;; File: ac-packages.el
;; Abner Chou's emacs modules
;; Installs any needed packages (usually only on first run).
;; Code adapted from prelude.el setup.

;; Required for running this code.
(require 'cl)

;; Define what packages are required from package.el.
(defvar required-packages
  '(
     use-package
    )
  "A list of packages to ensure are installed at launch.")

;; Function for determining if packages are installed.
(defun required-packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; Check which packages need to be installed and install them.
(unless (required-packages-installed-p)
  ;; Prevent auto-save-list while installing.
  (setq auto-save-list-file-name nil)

  ;; Check for new packages (package versions)
  (message "%s" "Updating package database...")
  (package-refresh-contents)
  (message "%s" " done.")

  ;; Install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p)))

  ;; Finally, if the compile-log window is active, kill it.
  (let ((buf (get-buffer "*Compile-Log*")))
    (when buf (delete-windows-on buf))))

(defun ac-load-packages ()
  (progn
    (use-package all-the-icons
      :ensure t)
    (use-package nlinum
      :ensure t
      :config
      (use-package nlinum-hl
        :ensure t
        :config
        (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
        (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)))
    ;; Evil
    (use-package evil
      :ensure t
      :bind (( "C-c SPC" . evil-avy-goto-word-1))
      :init
      (evil-mode 1)
      :config
      (use-package evil-indent-textobject
        :ensure t)
      (use-package evil-surround
        :ensure t
        :init
        (global-evil-surround-mode 1))
      (use-package evil-tutor
        :ensure t)

      ;; Basedon Xah's comments, no need multiple-cursors
      ;; Ref: http://ergoemacs.org/misc/emacs_multiple-cursors-mode.html
      ;; My experience is also negative using multiple-cusors.
      ;; (use-package evil-mc
      ;;   :ensure t
      ;;   :config
      ;;   (global-evil-mc-mode 1)
      ;;   (evil-define-key 'visual evil-mc-key-map
      ;;     "A" #'evil-mc-make-cursor-in-visual-selection-end
      ;;     "I" #'evil-mc-make-cursor-in-visual-selection-beg))

      (use-package evil-numbers
        :ensure t
        :bind (:map evil-normal-state-map
        ("C-c +" . evil-numbers/inc-at-pt)
        ("C-c -" . evil-numbers/dec-at-pt)
        :map evil-visual-state-map
        ("C-c +" . evil-numbers/inc-at-pt)
        ("C-c -" . evil-numbers/dec-at-pt)))

      (use-package evil-nerd-commenter
        :ensure t
        :init
        (evilnc-default-hotkeys))
      (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
    )
    ;; set up the dashboard for welcome
    (use-package dashboard
      :ensure t
      :bind (("<M-f8>" . switch-to-dashboard))
      :init
      (add-hook 'dashboard-mode-hook 'disable-global-nlinum-mode)
      (setq dashboard-startup-banner 'logo)
      (setq dashboard-set-heading-icons t)
      (setq dashboard-set-navigator t)
      (setq dashboard-items '((recents . 25)
            (bookmarks . 20)
            (projects . 25)
            (agenda . 15)
            (registers . 5)))
      (dashboard-setup-startup-hook))
    ;; Helm
    (use-package helm
      :ensure t
      :bind ( ("M-x" . helm-M-x)
              ("C-x r b" . helm-filtered-bookmarks)
              ("C-x C-f" . helm-find-files)
              ("C-x b" . helm-mini)
              ("C-x C-b" . helm-mini)
              ("C-c i" . helm-imenu))
      :init
      (helm-mode 1)
      :config
      (use-package helm-gtags
        :ensure t
        :hook ( (dired-mode . helm-gtags-mode)
                (eshell-mode . helm-gtags-mode)
                (c-mode . helm-gtags-mode)
                (c++-mode . helm-gtags-mode)
                (asm-mode . helm-gtags-mode))
        :config
        (setq
          helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t
        )
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
      (use-package helm-ag
        :ensure t
        :bind ("C-M-g" . helm-ag)
        )
      (use-package helm-swoop
        :ensure t)
      )

    ;; theme
    (use-package powerline
      :ensure t)
    (use-package solarized-theme
      :ensure t
      :init
      (load-theme 'solarized-dark t))
    (use-package smart-mode-line
      :ensure t
      :config
      (use-package smart-mode-line-powerline-theme
        :ensure t)
      (setq custom-safe-themes t)
      (setq sml/theme 'dark)
      (sml/setup))

    ;; Project/Directory
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
    ;; Program
    (use-package magit
      :ensure t
      :bind ( ("C-x g" . magit-status)
              ("C-x M-g" . magit-dispatch-popup))
      )
    ; Python mode add-on
    (use-package anaconda-mode
      :ensure t
      :hook python-mode
      :init
      (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

    (use-package cmake-mode
      :ensure t
      :defer t)

    (use-package exec-path-from-shell
      :ensure t
      :init
      (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize)))

    ;; Auto-complete
    ; Ref: https://github.com/kirang89/.emacs.d/blob/master/kiran/init-company.el
    (use-package company
      :ensure t
      :init
      (add-hook 'after-init-hook 'global-company-mode)
      (setq company-idle-delay 0.1
            company-echo-delay 0.1
            company-minimum-prefix-length 2
            company-show-numbers t
            company-tooltip-limit 20
            company-dabbrev-downcase nil
            company-dabbrev-ignore-case nil
            company-dabbrev-code-other-buffers t
            company-require-match 'never
            company-global-modes
            (global-company-mode +1))
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
        :bind (("C-c e" . company-elisp))
        :config
        (add-to-list 'company-backends 'company-elisp))
      (use-package company-gtags
        :bind (("C-c g" . company-gtags))
        :config
        (add-to-list 'company-backends 'company-gtags))
      (use-package company-yasnippet
        :config
        (add-to-list 'company-backends 'company-yasnippet))
      (use-package company-c-headers
        :ensure t
        :config
        (add-to-list 'company-backends 'company-c-headers))
      (use-package company-anaconda
        :ensure t
        :config
        (add-to-list 'company-backends 'company-anaconda))
      (use-package company-statistics
        :ensure t
        :config
        (add-hook 'after-init-hook 'company-statistics-mode))
      (use-package company-irony
        :ensure t
        :config
        (add-to-list 'company-backends 'company-irony))
      )

    ; Ref: https://github.com/jwiegley/dot-emacs/blob/master/init.el
    (use-package company-math
      :ensure t
      :defer t)

    (use-package company-quickhelp
      :ensure t
      :after company
      :bind (:map company-active-map
            ("C-c ?" . company-quickhelp-manual-begin)))

    (use-package irony
      :ensure t
      :hook ( (c++-mode . irony-mode)
              (c-mode . irony-mode)
              (objc-mode-hook . irony-mode)
              (irony-mode . irony-cdb-autosetup-compile-options))
      :init
      (setq w32-pipe-read-delay 0)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024))
    )

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
      :init
      (with-eval-after-load 'yasnippet
        (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))
      :config
      (validate-setq
      yas-verbosity 1
      yas-wrap-around-region t)
      (yas-reload-all)
      (yas-global-mode))

    (use-package yasnippet-snippets
      :ensure t
      :after yasnippet)

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

    (use-package editorconfig
      :ensure t
      :config
      (editorconfig-mode 1))

    (use-package keyfreq
      :ensure t
      :config
      (keyfreq-mode 1)
      (keyfreq-autosave-mode 1))

    (use-package avy
      :ensure t)

    (use-package jade-mode
      :ensure t)

    (use-package yaml-mode
      :ensure t)

    (use-package powershell
      :ensure t)
    )

    (use-package markdown-mode
      :ensure t
      :mode ( ("README\\.md\\'" . gfm-mode)
              ("\\.md\\'" . markdown-mode)
              ("\\.markdown\\'" . markdown-mode))
      :init
      (setq markdown-command "multimarkdown")
      (if (eq system-type 'windows-nt)
        (progn
          (setq markdown-command (concat (getenv "HOME") "\\scoop\\shims\\pandoc.exe"))
          )
        )
      )
    (use-package ztree
      :ensure t
      :config
      (evil-set-initial-state 'dashboard-mode 'emacs)
      )
  )

(provide 'ac-packages)
