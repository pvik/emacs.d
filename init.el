;; init.el --- Summary
;;; Commentary:
;;; Emacs init file

;;; Code:

;; turn up GC during startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar original--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; TODO
;; code folding - crashes org-mode export
;; snippets
;; python mode
;; remove - shackle & configure purpose
;; setup hydra

;; (load "~/.emacs.d/doom-core-slim")
(setq load-prefer-newer t) ;; load newer source instead of old bytecode
(add-to-list 'load-path "~/.emacs.d/modules/")
(require 'doom-core-slim)
(require 'private)
(require 'tbemail) ;; ThunderBird email mode

;; Display the total loading time in the minibuffer
(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "Initialized in %s" (emacs-init-time)))

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(setq-default
 inhibit-startup-screen t
 initial-scratch-message ";; Happy Hacking!!"
 left-margin-width 1 right-margin-width 1     ; Add left and right margins
 select-enable-clipboard t       ; Merge system's and Emacs' clipboard
 cursor-type '(bar . 2)          ; set cursor type to bar
 line-spacing pvik-line-spacing  ; line spacing - in private.el
 word-wrap t)

(global-visual-line-mode t)
(delete-selection-mode 1)        ; Replace region when inserting text
(global-hl-line-mode t)          ; Highlight current line
(fset 'yes-or-no-p 'y-or-n-p)    ; change all prompts to y or n

;; handling auto backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . "/tmp/")))
(setq auto-save-file-name-transforms
      `((".*" "/tmp/" t)))


;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("org"   . "https://orgmode.org/elpa/")
 			("gnu"   . "https://elpa.gnu.org/packages/")
 			("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Navigation
;; ==========

;; Smarter C-a (move to beginning of line)
(defun me/beginning-of-line-dwim ()
  "Move point to first non-whitespace character, or beginning of line."
  (interactive "^")
  (let ((origin (point)))
	(back-to-indentation)
	(let ((o2 (point)))
      (and (= origin o2)
		   (beginning-of-line)))))

(global-set-key (kbd "C-a")  #'me/beginning-of-line-dwim)

(global-set-key (kbd "M-s-]")  #'forward-paragraph)
(global-set-key (kbd "M-s-[")  #'backward-paragraph)

;; =======================================================
;; =======================================================

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'helm)
  :config
	(setq projectile-mode-line
				'(:eval (if (projectile-project-p)
										(format "[%s]"
														(projectile-project-name))
									"")))
	(projectile-mode 1))

;; Buffers

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-projectile
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

(defvar *protected-buffers* '("*scratch*" "*Messages*")
  "Buffers that cannot be killed.")

(defun my/protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(add-hook 'after-init-hook #'my/protected-buffers)

;; =======================================================
;; =======================================================

;; Email
;; =====

(use-package org-mime
  :ensure t	)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir (expand-file-name "~/mail/ctl"))
(setq mu4e-sent-folder "/Sent")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-trash-folder "/Trash")
(setq mu4e-inbox-folder "/Inbox")
;; smtp mail setting; these are the same that `gnus' uses.
(setq
 ;; authentication details set in ~/.authinfo
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "localhost"
   smtpmail-smtp-server         "localhost"
   smtpmail-local-domain        "centurylink.com"
	 smtpmail-smtp-service        1025)
;; give me ISO(ish) format date-time stamps in the header list
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)
;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; use pandoc for html email, it is much better than the default html2tex
;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
;; action to view in the browser:
(add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
;; smart refiling
(setq mu4e-refile-folder
			(lambda (msg)
				(cond
				 ;; report messages go to /auto-reports
				 ((mu4e-message-contact-field-matches msg :from
																							"mtb.bridge@centurylink.com")
					"/auto-reports")
				 ((mu4e-message-contact-field-matches msg :from
																							"AR_SCRIPT@centurylink.com")
					"/auto-reports")
				 ;; everything else goes to /archive
				 ;; important to have a catch-all at the end!
				 (t  "/archive/Inbox"))))
;; the maildirs you use frequently; access them with 'j' ('jump')
(setq   mu4e-maildir-shortcuts
				'(("/Inbox"       . ?i)
          ("/Sent"        . ?s)))
;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
(setq mu4e-headers-fields
			'( (:date          .  25)    ;; alternatively, use :human-date
				 (:flags         .   6)
				 (:from          .  22)
				 (:subject       .  nil))) ;; alternatively, use :thread-subject
; Program to get mail.
;; Called when 'U' is pressed in main view, or C-c C-u elsewhere
;; (setq mu4e-get-mail-command t)
;; (setq mu4e-update-interval 120)
;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-compose-reply-to-address "praveen.vikram@centurylink.com"
      user-mail-address "praveen.vikram@centurylink.com"
      user-full-name  "Praveen Vikram")
(setq mu4e-compose-signature
			"#+BEGIN_SRC\nThanks,\nPraveen Vikram\n#+END_SRC")
;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads")
;; split view # of lines to show in header view
(setq mu4e-headers-visible-lines 20)
;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)
;; stop editor from inserting line breaks
;; (add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
(setq org-mime-beautify-quoted-mail t)
(defun htmlize-before-send ()
    "When in an org-mu4e-compose-org-mode message, htmlize it."
    (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
      (message-mode)
      (org-mime-htmlize)))

(advice-add 'message-send-and-exit :before 'htmlize-before-send)

(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
			 (use-hard-newlines -1)))

(require 'org-mu4e)
;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; do not reply to self
(setq mu4e-compose-dont-reply-to-self t)
;; do not ask for confirmation on quit
(setq mu4e-confirm-quit nil)

(global-set-key (kbd "C-<f6>") 'mu4e)

;; https://matt.hackinghistory.ca/2016/11/18/sending-html-mail-with-mu4e/
;; https://github.com/djcb/mu/issues/392

;; handle signature
;;  (defun insert-mu4e-sig-here ()
;;     "Insert the mu4e signature here, assuming it is a string."
;;     (interactive)
;;     (save-excursion
;;       (when (stringp mu4e-compose-signature)
;;         (insert mu4e-compose-signature))))

;; (add-hook 'mu4e-compose-mode-hook 'insert-mu4e-sig-here)

;; Notifications
(use-package mu4e-alert
  :ensure t
  :config
	(mu4e-alert-set-default-style 'libnotify)
	(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
	(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
	(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"  ;; Unread of mail only in Inbox
			 " AND maildir:"
       "\"/Inbox\"")))

;; =======================================================
;; =======================================================

;; User Interface
;; ==============

(use-package alert
  :defer 1
  :custom (alert-default-style 'libnotify))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

;; All The Icons
(use-package all-the-icons
  :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :after all-the-icons
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (global-set-key [f6] 'neotree-toggle))

;; (use-package centaur-tabs
;;   :demand
;;   :init
;;   (defun centaur-tabs-buffer-groups ()
;; 	"`centaur-tabs-buffer-groups' control buffers' group rules.

;;     Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;;     All buffer name start with * will group to \"Emacs\".
;;     Other buffer group by `centaur-tabs-get-group-name' with project name."
;; 	(list
;; 	 (cond
;; 	  ((or (string-equal "*" (substring (buffer-name) 0 1))
;; 		   (memq major-mode '(magit-process-mode
;; 							  magit-status-mode
;; 							  magit-diff-mode
;; 							  magit-log-mode
;; 							  magit-file-mode
;; 							  magit-blob-mode
;; 							  magit-blame-mode
;; 							  )))
;; 	   "Emacs")
;; 	  ((derived-mode-p 'prog-mode)
;; 	   "Editing")
;; 	  ((derived-mode-p 'dired-mode)
;; 	   "Dired")
;; 	  ((memq major-mode '(helpful-mode
;; 						  help-mode))
;; 	   "Help")
;; 	  ((memq major-mode '(org-mode
;; 						  org-agenda-clockreport-mode
;; 						  org-src-mode
;; 						  org-agenda-mode
;; 						  org-beamer-mode
;; 						  org-indent-mode
;; 						  org-bullets-mode
;; 						  org-cdlatex-mode
;; 						  org-agenda-log-mode
;; 						  diary-mode))
;; 	   "OrgMode")
;; 	  (t
;; 	   (centaur-tabs-get-group-name (current-buffer))))))
;;   :hook
;;   (dired-mode . centaur-tabs-local-mode)
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-set-bar 'over)
;;   (setq centaur-tabs-modified-marker "*")
;;   :bind
;;   ("C-S-<iso-lefttab>" . centaur-tabs-backward)
;;   ("C-<tab>" . centaur-tabs-forward))

;; theme & modeline
;; ========
(use-package diminish
	:ensure t)

(column-number-mode 1)

(use-package doom-themes
  :ensure t
  :preface
  (defun my-init-theme (&optional _frame)
		(load-theme 'doom-one t)
		(doom-themes-visual-bell-config)
		(doom-themes-neotree-config)
		(doom-themes-org-config)
		(let ((line (face-attribute 'mode-line :underline)))
			(set-face-attribute 'mode-line          nil :overline   line)
			(set-face-attribute 'mode-line-inactive nil :overline   line)
			(set-face-attribute 'mode-line-inactive nil :underline  line)
			(set-face-attribute 'mode-line          nil :box        nil)
			(set-face-attribute 'mode-line-inactive nil :box        nil)
			(set-face-attribute 'mode-line          nil :height     pvik-modeline-active-font-height) ;; defined in private.el
			(set-face-attribute 'mode-line-inactive nil :height     pvik-modeline-inactive-font-height)
			;; (set-face-attribute 'mode-line          nil :foreground "#bbc2cf")
			;; (set-face-attribute 'mode-line          nil :background "#2257A0")
			;; (set-face-attribute 'mode-line-inactive nil :background "#21242b")
			)
		(set-face-attribute 'default nil :height pvik-default-font-height)
		(diminish 'flycheck-mode)
		(diminish 'purpose-mode)
		(diminish 'flyspell-mode)
		(diminish 'company-mode)
		(diminish 'yas-minor-mode)
		(diminish 'which-key-mode)
		(diminish 'helm-mode)
		(diminish 'eldoc-mode)
		(diminish 'smartparens-mode)
		(diminish 'outline-minor-mode)
		(diminish 'abbrev-mode)
		(diminish 'auto-revert-mode)
		(diminish 'auto-highlight-symbol-mode)
		(diminish 'aggressive-indent-mode)
		(diminish 'clj-refactor-mode))
  
  (defun my-reload-theme-in-daemon (frame)
		(when (or (daemonp) (not (display-graphic-p)))
      (with-selected-frame frame
				(run-with-timer 0.1 nil #'my-init-theme))))
  :init
  (setq doom-themes-enable-bold t  ; if nil, bold is universally disabled
				doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (add-hook 'after-make-frame-functions #'my-init-theme)
  (add-hook 'after-make-frame-functions #'my-reload-theme-in-daemon)
	
  ;; for GUI sessions
  (my-init-theme)
  (set-face-attribute 'default nil :height pvik-default-font-height))

(use-package spaceline
	:ensure t
  :config
	(require 'spaceline-config)
	(spaceline-emacs-theme)
	(spaceline-helm-mode)
	(spaceline-toggle-minor-modes-on)
  (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-workspace-number-on)
  (spaceline-toggle-evil-state-off)
  (spaceline-toggle-anzu-off)
  (spaceline-toggle-hud-off))

;; Which Key
(use-package which-key
  :ensure t
  :init
  ;; (setq which-key-popup-type 'side-window)
  ;; (setq which-key-separator " ")
  ;; (setq which-key-prefix-prefix "+")
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;; Window/Buffer management
;; ========================

;; kill this buffer key bind
(global-set-key (kbd "C-x C-k .") #'kill-this-buffer)

;; Shackle
;; (use-package shackle
;;   :ensure t
;;   :after helm
;;   :config
;;   (setq helm-display-function 'pop-to-buffer) ; make helm play nice
;;   (setq shackle-rules '(("*help*" :popup t :align below :ratio 0.2)
;; 												("*helm-mini*" :popup t :align left :ratio 0.4)
;; 												;;("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
;; 												("*cider-doc*" :popup t :align below :ratio 0.2)
;; 												("*cider-error*" :popup t :align below :ratio 0.35)
;; 												("\*cider-repl" :regexp t :popup t :align right :ratio 0.3 :inhibit-window-quit t)
;; 												("\*cljr" :regexp t :popup t :align below :ratio 0.35)))
;;   (setq shackle-lighter "")
;;   (shackle-mode))

;; purpose
(use-package window-purpose
  :ensure t
  :config
  (require 'window-purpose)
	;; play well with helm
	(setq purpose-preferred-prompt 'helm)
	(define-key purpose-mode-map (kbd "C-x b") nil)
	(define-key purpose-mode-map (kbd "C-x C-f") nil)
	(define-key purpose-mode-map (kbd "C-x C-k ,") 'purpose-x-popwin-close-windows)
	
	;; play well with magit
	(require 'window-purpose-x)
	(purpose-x-magit-single-on)
	
  (purpose-mode)
  ;; main 
	(add-to-list 'purpose-user-mode-purposes '(clojure-mode . main))
	(add-to-list 'purpose-user-mode-purposes '(web-mode . main))
	(add-to-list 'purpose-user-mode-purposes '(scss-mode . main))
	(add-to-list 'purpose-user-mode-purposes '(markdown-mode . main))
	;; repl
  (add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . repl))
	;; popups
	(add-to-list 'purpose-user-name-purposes '("*cider-?*" . popup))
	(add-to-list 'purpose-user-mode-purposes '(cider-stacktrace-mode . popup))
	
	(purpose-x-popwin-setup)
	;;(setq purpose-x-popwin-position 'bottom)
	;;(setq purpose-x-popwin-height 25)
	
  (purpose-compile-user-configuration))

;; eyebrowse
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package windmove
  :ensure t
  ;;:init
  ;;(global-set-key (kbd "C-M-<left>") #'windmove-left)
  ;;(global-set-key (kbd "C-M-<right>") #'windmove-right)
  :bind
  (("M-s-<left>". windmove-left)
   ("M-s-<right>". windmove-right)
   ("M-s-<up>". windmove-up)
   ("M-s-<down>". windmove-down)))

;; =======================================================
;; =======================================================

;; Helm
(defun elric//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :ensure t
  :bind (:map helm-map
							("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
							("C-i"   . helm-execute-persistent-action) ; make TAB work in terminal
							("C-z"   . helm-select-action)) ; list actions using C-z
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
  ;; (global-unset-key (kbd "C-x b"))
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  ;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-c h o") #'helm-occur)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
				helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
				helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
				helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
				helm-ff-file-name-history-use-recentf t
				helm-echo-input-in-header-line t)
  (add-hook 'helm-minibuffer-set-up-hook 'elric//helm-hide-minibuffer-maybe)
  (setq helm-boring-buffer-regexp-list
				(list
				 (rx "*magit-")
				 (rx "*helm")
				 (rx " ")
				 ;; circe buffers, use helm-circe instead
				 (rx "irc.freenode.net")
				 (rx "#clojure")
				 (rx "##java")))
  ;; (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  ;; (helm-autoresize-mode t)
  :config
  (add-hook 'helm-minibuffer-set-up-hook 'elric//helm-hide-minibuffer-maybe)
  (helm-autoresize-mode 1)
  (helm-mode 1))

;; helm-projectile
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  ;; :init (setq projectile-require-project-root nil)
  :init
  (global-set-key (kbd "C-x p") #'helm-projectile)
  :config
  (helm-projectile-on))

;; helm-tramp
(use-package helm-tramp
  :ensure t
  :config
  (add-hook 'helm-tramp-pre-command-hook '(lambda () (global-aggressive-indent-mode 0)
					    (projectile-mode 0)))
  (add-hook 'helm-tramp-quit-hook '(lambda () (global-aggressive-indent-mode 1)
				     (projectile-mode 1))))

(use-package exec-path-from-shell
  :ensure t)

;; =======================================================
;; =======================================================

;; tabs, spaces & indents
;; ======================

;; Enable tabs and set prefered indentation width in spaces
;; (In this case the indent size is 2-spaces wide)
(setq-default indent-tabs-mode t)
(setq-default standard-indent 4)
(setq-default tab-width 4)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:foreground "#636363")))))
;; (setq whitespace-display-mappings
;;       '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;; (global-whitespace-mode) ; Enable whitespace mode everywhere

;; Programming
;; ==========
(require 'programming)

;; magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; Restclient
(use-package restclient
	:ensure t)
(use-package company-restclient
	:ensure t)

;; plantuml mode
(use-package htmlize
	:ensure t)
(use-package plantuml-mode
  :ensure t)
(use-package flycheck-plantuml
  :ensure t)

;; LaTeX
;; =====
;; TODO: BibTex

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(use-package company-auctex
  :ensure t
  :after company)

(add-hook! (latex-mode LaTeX-mode) 'turn-on-auto-fill)
(add-hook! LaTeX-mode '(LaTeX-math-mode TeX-source-correlate-mode))

;; Data modes
;; ==========

(use-package json-mode
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer))))

(defun my/json-array-of-numbers-on-one-line (encode array)
  "Prints the arrays of numbers in one line."
  (let* ((json-encoding-pretty-print
          (and json-encoding-pretty-print
               (not (loop for x across array always (numberp x)))))
         (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
    (funcall encode array)))
:config
(advice-add 'json-encode-array :around #'my/json-array-of-numbers-on-one-line)

;; (use-package nxml
;;   :ensure t)
(use-package toml-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)

;; CSV
(use-package csv-mode
  :ensure t
  :config
  (setq-default csv-align-padding 2))

;; =======================================================
;; =======================================================

;; Text Editing & Regular usage
;; ============ = =============

;; pdf tools
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install))

;; org-mode
(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  (setq
   pvik--org-directory "~/Dropbox/org"
   pvik--notes-file (concat pvik--org-directory "/notes.org")
   pvik--work-notes-file (concat pvik--org-directory "/work.org"))
  (defun pvik--define-org-templates ()
    "Org capture tamplates."
    (setq org-capture-templates
					`(("t" "Todo" entry
						 (file+headline pvik--notes-file "Pending")
						 "* TODO %?  %U" :empty-lines 1)
						("n" "Note" entry
						 (file+headline pvik--notes-file "Notes")
						 "* NOTE %?\n  %U" :empty-lines 1)
						("w" "Work Todo" entry
						 (file+headline pvik--work-notes-file "Pending")
						 "* TODO %? %U" :empty-lines 1)
						("W" "Work Note" entry
						 (file+headline pvik--work-notes-file "Notes")
						 "* NOTE %?\n  %U" :empty-lines 1)
						("p" "Project Todo" entry
						 (file+headline ,(concat (projectile-project-root) "notes.org") "Pending")
						 "* TODO %?  %U\n%a" :empty-lines 1)
						("P" "Project Note" entry
						 (file+headline ,(concat (projectile-project-root) "notes.org") "Notes")
						 "* NOTE %?\n  %U\n%a" :empty-lines 1))))
  (defun pvik--org-capture ()
    "Org-capture to FILE-TO-CAPTURE."
    (interactive)
    (pvik--define-org-templates)
    (org-capture))
  (defun pvik--org-open-notes ()
    (interactive)
    (find-file pvik--notes-file))
  (defun pvik--org-open-work-notes ()
    (interactive)
    (find-file pvik--work-notes-file))
  (defun pvik--org-open-project-notes ()
    (interactive)
    (find-file (concat (projectile-project-root) "notes.org")))
  :mode
  ("\\.org$" . org-mode)
  :hook
  ((org-mode . org-sticky-header-mode)
   (org-mode . toc-org-enable))
  :config
  (pvik--define-org-templates)
  (setq-default
   org-log-done 'time
   org-descriptive-links nil
   org-support-shift-select 'always
   org-startup-folded nil
   org-startup-truncated nil
   org-directory (symbol-value 'pvik--org-directory)
   org-default-notes-file (symbol-value 'pvik--notes-file)) ;; More org-customization in pvik-core
  (global-set-key (kbd "C-c o c") #'pvik--org-capture)
  (global-set-key (kbd "C-c o r") #'org-refile)
  (global-set-key (kbd "C-c o o n") #'pvik--org-open-notes)
  (global-set-key (kbd "C-c o o w") #'pvik--org-open-work-notes)
  (global-set-key (kbd "C-c o o p") #'pvik--org-open-project-notes)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
	 (plantuml . t)
	 (python . t)
	 (shell . t)
	 (clojure . t)))
  (setq org-plantuml-jar-path
		(expand-file-name "~/.emacs.d/plantuml.jar"))
  (defun my-org-confirm-babel-evaluate (lang body)
	(not (string= lang "plantuml")))  ; don't ask for plantuml
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (setq org-html-checkbox-type 'html))
(use-package org-src
  :ensure nil
  :after org
  :config
  (setq-default
   org-edit-src-content-indentation 0
   org-edit-src-persistent-message nil
   org-src-window-setup 'current-window))
(use-package org-sticky-header
  :ensure t
  :config
  (setq-default
   org-sticky-header-full-path 'full
   org-sticky-header-outline-path-separator " / "))
(use-package toc-org
  :ensure t
  :after org)

;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq-default
   markdown-asymmetric-header t
   markdown-split-window-direction 'right))
(use-package markdown-toc
  :ensure t)

;; artist mode
(add-hook 'artist-mode-hook
					(lambda ()
						(local-set-key (kbd "<f1>") 'org-mode)
						(local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
            (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
						(local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
						(local-set-key (kbd "<f5>") 'artist-select-op-ellipse))); f5 = ellipse

;; =======================================================
;; =======================================================

;; w3m
;; =====
;; browser

(use-package w3m
  :ensure t
  :config
  (setq w3m-search-default-engine "duckduckgo")
  (setq w3m-default-desplay-inline-images t)) ;; Enable images in w3m


;; =======================================================
;; =======================================================

;; Circe
;; =====
;; IRC Client configuration

(use-package circe
  :ensure t
  :preface
  (defun circe-network-connected-p (network)
    "Return non-nil if there's any Circe server-buffer whose `circe-server-netwok' is NETWORK."
    (catch 'return
      (dolist (buffer (circe-server-buffers))
				(with-current-buffer buffer
          (if (string= network circe-server-network)
              (throw 'return t))))))
  (defun circe-maybe-connect (network)
    "Connect to NETWORK, but ask user for confirmation if it's already been connected to."
    (interactive "sNetwork: ")
    (if (or (not (circe-network-connected-p network))
            (y-or-n-p (format "Already connected to %s, reconnect?" network)))
				(circe network)))
  (defun pvik--irc ()
    "Connect to IRC"
    (interactive)
    ;; (circe "Bitlbee")
    (circe-maybe-connect "Freenode"))
  :config
  (setq circe-network-options
				`(("Freenode"
           :tls t
           :nick "madmonkey"
					 :nickserv-password ,freenode-password
					 :reduce-lurker-spam t
           :channels (;;"#emacs-circe"
					  :after-auth "#clojure" "#java" "#lisp")))))
(use-package helm-circe
  :ensure t
  :after (helm circe))
(use-package circe-notifications
  :ensure t
  :after (circe)
  :config
  (autoload 'enable-circe-notifications "circe-notifications" nil t)
  ;; (eval-after-load "circe-notifications"
  ;;   '(setq circe-notifications-watch-strings
  ;; 	   '("people" "you" "like" "to" "hear" "from")))
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))

(global-set-key (kbd "C-c c h") 'helm-circe)
(global-set-key (kbd "C-c c n") 'helm-circe-new-activity)

;; =======================================================
;; =======================================================

;; General Configurations
;; ======= ==============

;; linum mode for all files
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)

;; flyspell (fot text files)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(global-set-key (kbd "<f8>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; Desktop
(desktop-save-mode 1)

;; Default font
(set-default-font "Hack-9")

;; Emacs Personal Functions and Customization
;; ==========================================
(require 'pvik-core)

;; =======================================================
;; =======================================================

;; Reset GC back to normal
(add-hook! 'emacs-startup-hook
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook! 'emacs-startup-hook
  (setq file-name-handler-alist original--file-name-handler-alist))

;; garbage collect on focus out
(add-hook 'focus-out-hook #'garbage-collect)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style (quote libnotify))
 '(custom-safe-themes
   (quote
	("4e10cdf7d030fb41061cf57c74f6ddfc19db8d4af6c8e0723dc77f9922543a3d" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "80365dd15f97396bdc38490390c23337063c8965c4556b8f50937e63b5e9a65c" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(package-selected-packages
   (quote
	(slime-company slime sql-indent centaur-tabs company-go htmlize dockerfile-mode helm-posframe posframe go-mode company-distel diminish moody flycheck-nim nim-mode ac-geiser geiser flycheck-rust window-purpose w3m fill-column-indicator circe org spaceline-config eyebrowse helm-purpose scad-preview scad-mode spaceline neotree projectile which-key helm doom-themes use-package)))
 '(safe-local-variable-values
   (quote
	((org-edit-src-content . 0)
	 (org-src-preserve-indentation)))))

;; fonts
(set-face-attribute 'default nil :height pvik-default-font-height)
(set-face-attribute 'mode-line          nil :height     pvik-modeline-active-font-height) ;; defined in private.el
(set-face-attribute 'mode-line-inactive nil :height     pvik-modeline-inactive-font-height)
(setq-default line-spacing pvik-line-spacing)

;;; init.el ends here

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ahs-face ((t (:background "#3e4147" :foreground "#bbc2cf"))))
;;  '(ahs-plugin-defalt-face ((t (:background "#8795af" :foreground "Black")))))
