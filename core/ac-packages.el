;; File: ac-packages.el
;; Abner Chou's emacs modules
;; Installs any needed packages (usually only on first run).
;; Code adapted from prelude.el setup.

;; Required for running this code.
(require 'cl-lib)

;; Define what packages are required from package.el.
;; (defvar required-packages
;;   '(
;;      use-package
;;     )
;;   "A list of packages to ensure are installed at launch.")

;; Function for determining if packages are installed.
;; (defun required-packages-installed-p ()
;;   (loop for p in required-packages
;;         when (not (package-installed-p p)) do (return nil)
;;         finally (return t)))

;; Check which packages need to be installed and install them.
;; (unless (required-packages-installed-p)
;;   ;; Prevent auto-save-list while installing.
;;   (setq auto-save-list-file-name nil)

;;   ;; Check for new packages (package versions)
;;   (message "%s" "Updating package database...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")

;;   ;; Install the missing packages
;;   (dolist (p required-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p)))

;;   ;; Finally, if the compile-log window is active, kill it.
;;   (let ((buf (get-buffer "*Compile-Log*")))
;;     (when buf (delete-windows-on buf))))

(defun ac-load-packages ()
  (progn

    (use-package auto-compile ; Need to keep this at top to enable auto-compile as early as possible
      :init
      (auto-compile-on-load-mode)
      (auto-compile-on-save-mode)
      :config
      (setq auto-compile-display-buffer nil)
      (setq auto-compile-mode-line-counter t)
      )

    (use-package all-the-icons)

    (use-package display-line-numbers
      :config
      (setq display-line-numbers-type t)
      (setq display-line-numbers-grow-only t)
      (global-display-line-numbers-mode 1))

    (use-package eldoc
      :delight eldoc-mode)

    ;; Evil
    (setq evil-want-keybinding nil)
    (use-package evil
      :init
      (evil-mode 1)
      :config
      (use-package evil-indent-textobject)
      (use-package evil-surround
        :init
        (global-evil-surround-mode 1)
        :config
        (add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
        )

      (use-package evil-tutor
        :commands evil-tutor)

      ;; Basedon Xah's comments, no need multiple-cursors
      ;; Ref: http://ergoemacs.org/misc/emacs_multiple-cursors-mode.html
      ;; My experience is also negative using multiple-cursors.
      ;; (use-package evil-mc
      ;;   :ensure t
      ;;   :config
      ;;   (global-evil-mc-mode 1)
      ;;   (evil-define-key 'visual evil-mc-key-map
      ;;     "A" #'evil-mc-make-cursor-in-visual-selection-end
      ;;     "I" #'evil-mc-make-cursor-in-visual-selection-beg))

      (use-package evil-numbers
        :bind (:map evil-normal-state-map
        ("C-c +" . evil-numbers/inc-at-pt)
        ("C-c -" . evil-numbers/dec-at-pt)
        :map evil-visual-state-map
        ("C-c +" . evil-numbers/inc-at-pt)
        ("C-c -" . evil-numbers/dec-at-pt)))

      (use-package evil-nerd-commenter
        :init
        (evilnc-default-hotkeys))
      (use-package evil-escape
        :init
        (evil-escape-mode)
        :config
        (setq-default evil-escape-key-sequence "jk")
        (setq-default evil-escape-delay 0.1)
        )
      (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
    )
    ;; set up the dashboard for welcome
    (use-package dashboard
      :bind (("<M-f8>" . switch-to-dashboard))
      :init
      (add-hook 'dashboard-mode-hook 'disable-global-display-line-numbers-mode)
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
      :bind ( ("M-x" . helm-M-x)
              ("C-x r b" . helm-filtered-bookmarks)
              ("C-x C-f" . helm-find-files)
              ("C-x b" . helm-mini)
              ("C-x C-b" . helm-mini)
              ("C-c i" . helm-semantic-or-imenu)
              ("C-c l" . helm-locate)
              ("C-c b" . helm-resume)
              ("M-y" . helm-show-kill-ring)
              )
      :init
      (helm-mode 1)
      :config
      (when IS-WIN
        (setq helm-locate-command "es %s -sort run-count %s")
        (defun helm-es-hook ()
          (when (and (equal (assoc-default 'name (helm-get-current-source)) "Locate")
                    (string-match "\\`es" helm-locate-command))
            (mapc (lambda (file)
                    (call-process "es" nil nil nil
                                  "-inc-run-count" (convert-standard-filename file)))
                  (helm-marked-candidates))))
        (add-hook 'helm-find-many-files-after-hook 'helm-es-hook)
        )
      (use-package helm-gtags
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
      ;; (use-package helm-ag
      ;;   :bind ("C-M-g" . helm-ag)
      ;;   )
      ;; (use-package helm-rg
      ;;   :bind ("C-M-g" . helm-rg))
      (use-package helm-swoop
        :bind ( ("M-i" . helm-swoop)
                ("M-I" . helm-swoop-back-to-last-point)
                ("C-c M-i" . helm-multi-swoop)
                ("C-x M-i" . helm-multi-swoop-all)
                )
        :config
        (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
        (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
        ;; Move up and down like isearch
        (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
        (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
        (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
        (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

        ;; Save buffer when helm-multi-swoop-edit complete
        (setq helm-multi-swoop-edit-save nil)
        ;; If this value is t, split window inside the current window
        (setq helm-swoop-split-with-multiple-windows nil)
        ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
        (setq helm-swoop-split-direction 'split-window-horizontally)
        ;; If nil, you can slightly boost invoke speed in exchange for text color
        ;; (setq helm-swoop-speed-or-color nil)
        ;; ;; Go to the opposite side of line from the end or beginning of line
        (setq helm-swoop-move-to-line-cycle t)
        ;; Optional face for line numbers
        ;; Face name is `helm-swoop-line-number-face`
        ;; (setq helm-swoop-use-line-number-face t)
        ;; If you prefer fuzzy matching
        ;; (setq helm-swoop-use-fuzzy-match t)
        )
    )

    (use-package rg
      :bind ( ("C-M-g" . rg-dwim)
              ("<M-f3>" . rg-search-project)
              )
      :config
      (rg-define-search rg-search-project
        "Search project without asking for the file type"
        :files current
        :dir project)
      )

    ;; To enable flyspell, the backend aspell should be installed.
    (use-package flyspell-correct-helm
        :config
        (when IS-WIN (setq ispell-program-name "~/.emacs.d/thirdparties/win/hunspell-1.3.2-3-w32/bin/hunspell.exe"))
        (when IS-MAC (setq ispell-program-name "/usr/local/bin/aspell"))
        (setq ispell-local-dictionary "en_US")
        (setq ispell-local-dictionary-alist
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
      )

    ;; theme
    (use-package powerline)
    (use-package solarized-theme
      :init
      (load-theme 'solarized-dark t))
    (use-package smart-mode-line
      :config
      (use-package smart-mode-line-powerline-theme)
      (setq custom-safe-themes t)
      (setq sml/theme 'dark)
      (sml/setup))

    ;; Project/Directory
    (use-package projectile
      :bind ( ("C-x P" . projectile-switch-open-project)
              ("C-x p" . projectile-switch-project)
              ;; ("<M-f3>" . projectile-ag)
              )
      :config
      (projectile-global-mode)
      (setq projectile-enable-caching t)
      (setq projectile-indexing-method 'hybrid)
      ;; (use-package ag)
      ;; Use ripgrep as difault search engine
      (use-package helm-projectile
        :bind ( ("C-c f" . helm-projectile-find-file)
                ;; ("<M-f3>" . helm-projectile-rg)
                )
	)
      )

    (use-package neotree
      :bind ("<f8>" . neotree-toggle)
      :init
      (add-hook 'neotree-mode-hook 'disable-global-display-line-numbers-mode)
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
      :bind ( ("C-x g" . magit-status)
              ("C-x M-g" . magit-dispatch-popup))
      )
    ; Python mode add-on
    (use-package python
      :mode ("\\.py\\'" . python-mode)
      :interpreter (("python"  . python-mode)
                    ("python2" . python-mode)
                    ("python3" . python-mode))
      :hook (python-mode . subword-mode)
      )
    (use-package python-switch-quotes
      :after python
      :bind ( :map python-mode-map
              ("C-c '" . python-switch-quotes))
      )
    (use-package anaconda-mode
      :defer t
      :after python-mode
      :hook python-mode
      :init
      (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
      (setq anaconda-mode-localhost-address "localhost")
      )

    (use-package cmake-mode
      :defer t)

    (use-package exec-path-from-shell
      :init
      (when IS-MAC (exec-path-from-shell-initialize)))

    ;; Auto-complete
    ; Ref: https://github.com/kirang89/.emacs.d/blob/master/kiran/init-company.el
    (use-package company
      :delight company-mode
      :init
      (add-hook 'after-init-hook 'global-company-mode)
      (setq company-idle-delay 0
            company-echo-delay 0
            company-minimum-prefix-length 1
            company-show-numbers t
            company-tooltip-limit 20
            company-dabbrev-downcase nil
            company-dabbrev-ignore-case nil
            company-dabbrev-code-other-buffers t
            company-require-match 'never)
      (company-tng-configure-default)
      (global-company-mode)
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
        :bind (("C-c w" . company-web-html))
        :config
        (add-to-list 'company-backends 'company-web-html))
      (use-package company-elisp
        :ensure nil
        :bind (("C-c e" . company-elisp))
        :config
        (add-to-list 'company-backends 'company-elisp))
      (use-package company-gtags
        :ensure nil
        :bind (("C-c g" . company-gtags))
        :config
        (add-to-list 'company-backends 'company-gtags))
      (use-package company-yasnippet
        :ensure nil
        :defer t
        :config
        (add-to-list 'company-backends 'company-yasnippet))
      (use-package company-c-headers
        :defer t
        :config
        (add-to-list 'company-backends 'company-c-headers))
      (use-package company-anaconda
        :defer t
        :after anaconda-mode
        :config
        (add-to-list 'company-backends 'company-anaconda))
      (use-package company-statistics
        :config
        (add-hook 'after-init-hook 'company-statistics-mode))
      (use-package company-irony
        :defer t
        :after irony-mode
        :config
        (add-to-list 'company-backends 'company-irony))
      )

    ; Ref: https://github.com/jwiegley/dot-emacs/blob/master/init.el
    (use-package company-math
      :defer t)

    (use-package company-quickhelp
      :after company
      :bind (:map company-active-map
            ("C-c ?" . company-quickhelp-manual-begin)))

    (use-package irony
      :hook ( (c++-mode . irony-mode)
              (c-mode . irony-mode)
              (objc-mode-hook . irony-mode)
              (irony-mode . irony-cdb-autosetup-compile-options))
      :init
      (setq w32-pipe-read-delay 0)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024))
    )

    (use-package validate)

    (use-package smartparens
      :after validate
      :config
      (show-smartparens-global-mode 1)
      (smartparens-global-mode 1))

    (use-package yasnippet
      :defer t
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
      :defer t
      :after yasnippet)

    (use-package p4
      :config
      (p4-set-p4-config "p4config"))

    (use-package dired-hacks-utils
      :defer t
      :config
      (use-package dired-filter)
      (use-package dired-subtree
        :init
        (evil-define-key 'normal dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
        (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-subtree-up)
        (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-subtree-down))
      (use-package dired-open)
      (use-package dired-avfs
        :if (executable-find "mountavfs"))
      (use-package dired-rainbow
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
      (use-package dired-ranger)
      (use-package dired-narrow)
      ;; (use-package dired-list
        ;; :ensure t)
      (use-package dired-collapse
        :init
        (add-hook 'dired-mode-hook (lambda () (dired-collapse-mode 1))))
    )

    (use-package peep-dired
      :hook (peep-dired-hook . evil-normalize-keymaps)
      :init
      (evil-define-key 'normal dired-mode-map (kbd "<SPC>") 'peep-dired)
      (evil-define-key 'normal peep-dired-mode-map (kbd "-") 'peep-dired-scroll-page-down)
      (evil-define-key 'normal peep-dired-mode-map (kbd "=") 'peep-dired-scroll-page-up)
      ;; (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
      ;; (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
      :config
      (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "mp3" "exe" "dll" "obj" "o" "pdb"))
      )

    (use-package editorconfig
      :delight editorconfig-mode
      :config
      (editorconfig-mode 1))

    (use-package keyfreq
      :config
      (keyfreq-mode 1)
      (keyfreq-autosave-mode 1))

    (use-package avy
      :defer t)

    (use-package jade-mode
      :defer t)

    (use-package yaml-mode
      :defer t
      :mode "\\.yml\\'"
      )

    (use-package powershell
      :defer t)

    (use-package markdown-mode
      :defer t
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
      :defer t
      :config
      (evil-set-initial-state 'dashboard-mode 'emacs)
      )

    (use-package flycheck
      :defer t
      :delight flycheck-mode
      :config
      (setq-default flycheck-disabled-checkers '( emacs-lisp-checkdoc
                                                  python-flake8
                                                  json-python-json
                                                  ))
      (global-flycheck-mode)
      )

    (use-package hl-todo
      :config
      (global-hl-todo-mode))

    (use-package which-key
      :delight which-key-mode
      :config
      (setq which-key-sort-order #'which-key-prefix-then-key-order
            which-key-sort-uppercase-first nil
            which-key-add-column-padding 1
            which-key-max-display-columns nil
            which-key-min-display-lines 6
            which-key-side-window-slot -10)
      (which-key-mode 1)
      )

    (use-package helpful
      :bind ( ("C-h f" . helpful-callable)
              ("C-h C" . helpful-command)
              ("C-h F" . helpful-function)
              ("C-h v" . helpful-variable)
              ("C-h k" . helpful-key)
              ("C-h ." . helpful-at-point))
      )

    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init)
      )

    (use-package hydra
      :config
      (require 'hydra-space)
      ;; (define-key evil-normal-state-map (kbd "<SPC>")
      ;;   (lambda ()
      ;;     (interactive)
      ;;     (evil-without-repeat
      ;;       (call-interactively #'my-hydra-space/body))))
      )

    (use-package general
      :init
      (setq general-override-states '(insert
                                      emacs
                                      hybrid
                                      normal
                                      visual
                                      motion
                                      operator
                                      replace))
      (general-define-key
        :states '(normal visual motion)
        :keymaps 'override
        "SPC" 'hydra-space/body)
      )

    (use-package leetcode
      :commands leetcode
      :config
      (setq leetcode-prefer-language "python3")
      (setq leetcode-prefer-sql "mysql")
      )

    )
  )

(provide 'ac-packages)
