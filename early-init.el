;;;; -*- lexical-binding: t; -*-
;; From: https://github.com/jojojames/.emacs.d/blob/2018.1/early-init.el

(require 'package)

;; Defined in Emacs 27 and above.
(defvar package-quickstart)

;; Separate package directories according to Emacs version.
;; Bytecode compiled in different Emacs versions are not
;; guaranteed to work with another.
(setq package-user-dir
      (format "%selpa/%s/" user-emacs-directory emacs-major-version))

;; Package Repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(when (and IS-WIN (not (gnutls-available-p)))
  (progn
    (setcar package-archives '("melpa" . "http://melpa.org/packages/"))
    (setcar (cdr package-archives) '("melpa-stable" . "http://stable.melpa.org/packages/"))
    )
  )
(when (< emacs-major-version 24)
  ;; for important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(when (and (eq emacs-version "26.2") IS-MAC)
  ;; fix a bug that ELPA bad request
  (progn
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
    (setcar (cddr package-archives) '("gnu" . "http://elpa.gnu.org/package/"))
    )
  )

(setq package-archive-priorities '(("org" . 15)
                                   ("melpa" . 10)
                                   ("melpa-stable" . 5)
                                   ("gnu" . 1)))

;; Activate all packages (in particular autoloads).
;; Use `package-quickstart' feature in Emacs 27 so we only need to
;; `package-initialize' if on Emacs 26 and below.
;; Take a look at $EMACS_CODEBASE/lisp/startup.el to refresh your memory.
;; The gist is that `package-activate-all' is called in Emacs 27 which
;; reads `package-quickstart'.
(if (>= emacs-major-version 27)
    (setq package-quickstart t)
  (package-initialize))

;; We provide `early-init' so that Emacs 26 and below can reuse this file.
;; In Emacs 27, this file is loaded automatically.
(provide 'early-init)
