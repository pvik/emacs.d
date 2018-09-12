;;; pvik-core.el --- personal code  -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom Emacs functions and helpers.

;;; Code:

;; defunct - using hooks from doom-themes to reload theme
;; (defun pvik--reload-theme ()
;;   "Helper fn to reload theme."
;;   (interactive)
;;   (load-theme 'doom-one t))

;; (defun pvik--org-capture (file-to-capture)
;;   "Org-capture to FILE-TO-CAPTURE."
;;   (interactive)
;;   (setq org-default-notes-file file-to-capture)
;;   (org-capture)
;;   (setq org-default-notes-file (symbol-value 'pvik--notes-file)))

;; (defun pvik--org-capture-work ()
;;   "Org-Capture to Work notes."
;;   (interactive)
;;   (pvik--org-capture (symbol-value 'pvik--work-notes-file)))

;; (defun pvik--org-capture-project ()
;;   "Org-Capture to project root."
;;   (interactive)
;;   (pvik--org-capture (concat (projectile-project-root) "notes.org")))

;; org-capture work
;;(global-set-key (kbd "C-c o c w") #'pvik--org-capture-work)
;; org-capture project
;;(global-set-key (kbd "C-c o c p") #'pvik--org-capture-project)

(provide 'pvik-core)

;;; pvik-core.el ends here
