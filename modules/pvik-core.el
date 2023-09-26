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
;;   (setq org-default-notes-file (symbol-value 'pvik-default-notes-file)))

;; (defun pvik--org-capture-work ()
;;   "Org-Capture to Work notes."
;;   (interactive)
;;   (pvik--org-capture (symbol-value 'pvik-default-work-notes-file)))

;; (defun pvik--org-capture-project ()
;;   "Org-Capture to project root."
;;   (interactive)
;;   (pvik--org-capture (concat (projectile-project-root) "notes.org")))

;; org-capture work
;; (global-set-key (kbd "C-c o c w") #'pvik--org-capture-work)
;; org-capture project
;; (global-set-key (kbd "C-c o c p") #'pvik--org-capture-project)

(defvar pvik-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o n")  (lambda () (interactive) (find-file pvik-default-notes-file)))
	(define-key map (kbd "C-c o w")  (lambda () (interactive) (find-file pvik-default-work-notes-file)))
	(define-key map (kbd "C-c o p") (lambda () (interactive) (find-file (concat (projectile-project-root) "notes.org"))))
    map)
  "pvik-keys-minor-mode keymap.")

(define-minor-mode pvik-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " pvik-keys")

(provide 'pvik-core)

;;; pvik-core.el ends here
