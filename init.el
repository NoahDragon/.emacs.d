;; init.el --- Where all them magic begins
;;
;; Get rid of the .emacs to be more version control friendly.
;; All settings should goes here, or sub files.
;; 

;; set MELPA
(require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/pakcages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))

;; Set Evil Mode
(require 'evil)
  (evil-mode 1)

;; Set Font that support Chinese character
(add-to-list 'default-frame-alist '(font . "YaHei Consolas Hybrid"))
