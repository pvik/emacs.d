;;; frames.el --- Define and configure frame related stuff

;;; Commentary:

;;; Code:

(defun pvik/split-windows()
  "Split windows my way."
  (interactive)
  ;; remove other frames
  (delete-other-windows)
  ;; Create new window right of the current one
  ;; Current window is 80 characters (columns) wide
  (split-window-right 100)
  ;; Go to next window
  (other-window 1)
  ;; Create new window below current one
  (split-window-below)
  ;; Start eshell in current window
  ;; (eshell)
  ;; Go to previous window
  (other-window -1)
  ;; never open any buffer in window with shell
  ;; (set-window-dedicated-p (nth 1 (window-list)) t)
  )

(defun pvik/move-buf-to-next-window ()
  "Move the current buffer to the next frame."
  (interactive)
  (let* ((this (selected-window))
		 (other (next-window))
		 (this-buffer (window-buffer this)))
	(set-window-buffer other this-buffer)))

(defun pvik/move-buf-to-previous-window ()
  "Move the current buffer to the next frame."
  (interactive)
  (let* ((this (selected-window))
		 (other (previous-window))
		 (this-buffer (window-buffer this)))
	(set-window-buffer other this-buffer)))

(defun pvik/swap-buf-with-next-window ()
  "Swap the current buffer with the buffer in the next frame."
  (interactive)
  (let* ((this (selected-window))
		 (other (next-window))
		 (this-buffer (window-buffer this))
		 (other-buffer (window-buffer other)))
	(set-window-buffer other this-buffer)
	(set-window-buffer this other-buffer)))

(defvar pvik/help-temp-buffers '("^\\*Flycheck errors\\*$"
                                 "^\\*Completions\\*$"
                                 "^\\*Help\\*$"
                                 ;; Other buffers names...
                                 "^\\*Colors\\*$"
								 "^\\*rust-analyzer\\*$"
								 "^\\*rustic-compilation\\*$"
                                 "^\\*Async Shell Command\\*$"))

(defvar pvik/repl-buffers '("^\\*slime-repl sbcl\\*$"
							"^\\*inferior-lisp\\*$"))

(defun pvik/display-buffer (buffer &optional alist)
  "Select window for BUFFER (need to use word ALIST on the first line).
Returns third visible window if there are three visible windows, nil otherwise.
Minibuffer is ignored."
  (let ((wnr (if (active-minibuffer-window) 3 2)))
    (when (= (+ wnr 1) (length (window-list)))
      (let ((window (nth wnr (window-list))))
        (set-window-buffer window buffer)
        window)))
  )

(while pvik/help-temp-buffers
  (add-to-list 'display-buffer-alist
               `(,(car pvik/help-temp-buffers)
                 (display-buffer-reuse-window
                  pvik/display-buffer
                  display-buffer-in-side-window)
                 (reusable-frames     . visible)
                 (side                . bottom)
                 (window-height       . 0.33)
                 ))
  (setq pvik/help-temp-buffers (cdr pvik/help-temp-buffers)))

(defun pvik/display-repl-buffer (buffer &optional alist)
  "Select window for BUFFER (need to use word ALIST on the first line).
Returns second visible window if there are three visible windows, nil otherwise.
Minibuffer is ignored."
  (let ((wnr (if (active-minibuffer-window) 2 1)))
    (when (= (+ wnr 2) (length (window-list)))
      (let ((window (nth wnr (window-list))))
        (set-window-buffer window buffer)
        window))))

(while pvik/repl-buffers
  (add-to-list 'display-buffer-alist
               `(,(car pvik/repl-buffers)
                 (display-buffer-reuse-window
                  pvik/display-repl-buffer
                  display-buffer-in-side-window)
                 (reusable-frames     . visible)
                 (side                . top)
                 (window-height       . 0.33)
                 ))
  (setq pvik/repl-buffers (cdr pvik/repl-buffers)))


(global-set-key (kbd "C-c C-f C-s")  #'pvik/split-windows)

(global-set-key (kbd "C-c C-f C-n")  #'next-multiframe-window)
(global-set-key (kbd "C-c C-f C-b")  #'previous-multiframe-window)

(global-set-key (kbd "C-c C-f C-f")  #'pvik/move-buf-to-next-window)
(global-set-key (kbd "C-c C-f C-p")  #'pvik/move-buf-to-previous-window)
(global-set-key (kbd "C-c C-f C-w")  #'pvik/swap-buf-with-next-window)

(provide 'frames)
;;; frames.el ends here
