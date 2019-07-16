;; File: ac-funcitons.el
;; Abner Chou's emacs modules
;; Customized functions.

(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

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

(provide 'ac-functions)
