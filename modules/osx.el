;;; osx.el --- osx specific code  -*- lexical-binding: t; -*-

;;; Commentary:
;; OSX Specific code

;;; Code:

;; This works for copying, but not pasting for some reason
(setq select-enable-clipboard t)

;; Whatever... it's easy enough to implement that part ourselves
(defun pvik/interprogram-paste-function ()
  "Paste in terminal."
  (interactive "^")
  (insert (shell-command-to-string "pbpaste")))

(global-set-key (kbd "s-v")  #'pvik/interprogram-paste-function)

(provide 'osx)

;;; osx.el ends here
