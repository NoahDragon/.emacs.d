;; File: ac-funcitons.el
;; Abner Chou's emacs modules
;; Customized functions.

(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(defun disable-global-linum-mode()
  "Disable global-linum-mode."
  (add-hook 'after-change-major-mode-hook
    (lambda () (linum-mode 0))
    :append :local))

(defun disable-global-nlinum-mode()
  "Disable global-nlinum-mode."
  (add-hook 'after-change-major-mode-hook
    (lambda () (nlinum-mode 0))
    :append :local))

;; Buffer operations
;; Ref: https://www.emacswiki.org/emacs/KillingBuffers
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun close-and-kill-this-pane ()
    "If there are multiple windows, then close this pane and kill the buffer in it also."
    (interactive)
    (kill-this-buffer)
    (if (not (one-window-p))
	(delete-window)))

(defun close-and-kill-next-pane ()
    "If there are multiple windows, then close the other pane and kill the buffer in it also."
    (interactive)
    (other-window 1)
    (kill-this-buffer)
    (if (not (one-window-p))
	(delete-window)))

(defun switch-to-dashboard ()
  "Switch to dashboard, should install the dashboard plugin."
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun next-user-buffer ()
  "Next buffer, but skip starred buffers."
  (interactive)
  (let (( bread-crumb (buffer-name)))
    (next-buffer)
    (while
	(and
	 (string-match-p "^\*" (buffer-name))
	 (not ( equal bread-crumb (buffer-name))))
      (next-buffer))))

(defun prev-user-buffer ()
  "Previous buffer, but skip starred buffers."
  (interactive)
  (let (( bread-crumb (buffer-name)))
    (previous-buffer)
    (while
	(and
	 (string-match-p "^\*" (buffer-name))
	 (not ( equal bread-crumb (buffer-name))))
      (previous-buffer))))

;; From: http://ix.io/SpI
;; Eshell, I love you
(defun eshell-throwaway ()
  (interactive)
  (eshell (cdr (cdr (cdr (current-time))))))

(defun martin-eshell ()
  (interactive)
  (if (projectile-project-p)
      (projectile-run-eshell)
    (eshell)))

(global-set-key (kbd "C-x m") #'martin-eshell)
(global-set-key (kbd "C-x M-m") #'eshell-throwaway)

(defun eshell/vim (filename &optional wildcards)
  (if wildcards
      (find-file filename wildcards)
    (find-file filename)))

(defun eshell/emacs (filename &optional wildcards)
  (if wildcards
      (find-file filename wildcards)
    (find-file filename)))

(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))
;; End From

;; From: https://www.emacswiki.org/emacs/IncrementNumber
(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
; Hex increment
(defun my-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))
(defun my-increment-number-hexadecimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789abcdefABCDEF")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
              (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match (format (concat "%0" (int-to-string field-width)
                                         hex-format)
                                 answer)))))))
; Binary increment
(defun my-format-bin (val width)
  "Convert a number to a binary string."
  (let (result)
    (while (> width 0)
      (if (equal (mod val 2) 1)
          (setq result (concat "1" result))
        (setq result (concat "0" result)))
      (setq val (/ val 2))
      (setq width (1- width)))
    result))

(defun my-increment-number-binary (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "01")
        (when (re-search-forward "[0-1]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 2) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 2 field-width) answer)))
          (replace-match (my-format-bin answer field-width)))))))
; VIM-like keybindings
(defun my-change-number-at-point (change)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (progn
        (forward-word)
        (search-backward (number-to-string number))
        (replace-match (number-to-string (funcall change number)))
        (goto-char point)))))
(defun my-increment-number-at-point ()
  "Increment number at point like vim's C-a"
  (interactive)
  (my-change-number-at-point '1+))
(defun my-decrement-number-at-point ()
  "Decrement number at point like vim's C-x"
  (interactive)
  (my-change-number-at-point '1-))
;; End From

;; From https://stackoverflow.com/questions/730751/hiding-m-in-emacs
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(provide 'ac-functions)
