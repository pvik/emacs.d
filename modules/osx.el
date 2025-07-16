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

(when (eq system-type 'darwin)
  (progn
	;; (require 'mac-notifications)
	
	(setq mac-option-key-is-meta t
          mac-command-key-is-meta nil
          ;; mac-command-modifier 'meta
          mac-option-modifier 'meta
		  )
	
	(global-set-key (kbd "s-v")  #'pvik/interprogram-paste-function)))

(provide 'osx)

;;; osx.el ends here
