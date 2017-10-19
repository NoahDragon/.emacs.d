;; File: init-packages.el
;; Installs any needed packages (usually only on first run).
;; Code adapted from prelude.el setup.

;; Required for running this code.
(require 'cl)
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

;; Initialise the package system.
(package-initialize)

;; Define what packages are required from package.el.
(defvar required-packages
  '(
    dashboard
    evil
    evil-magit
    helm
    magit
    markdown-mode
    monokai-theme
    powerline
    projectile
    smart-mode-line
    smart-mode-line-powerline-theme
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

(provide 'init-packages)
