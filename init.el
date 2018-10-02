;;; init.el --- Summary
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
 line-spacing pvik-line-spacing) ; line spacing - in private.el

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
  (projectile-mode 1))

;; =======================================================
;; =======================================================

;; User Interface
;; ==============

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

;; theme & modeline
;; ========

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
			(set-face-attribute 'mode-line          nil :foreground "#bbc2cf")
			(set-face-attribute 'mode-line          nil :background "#2257A0")
			(set-face-attribute 'mode-line-inactive nil :background "#21242b"))
		(set-face-attribute 'default nil :height pvik-default-font-height))
  
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
  (my-init-theme))

(use-package moody
  :ensure t
	:preface
	(defvar pv-mode-line-mode
		'(:eval (moody-tab
						 (format-mode-line
							(format "%s" mode-name))
						 nil 'up)))
	(put 'pv-mode-line-mode 'risky-local-variable t)
	(make-variable-buffer-local 'pv-mode-line-mode)
	
	(defvar pv-mode-line-buffer-identification
		'(:eval (moody-tab
						 (format-mode-line
							(if (projectile-project-p)
									(propertized-buffer-identification
									 (concat "%b" (format " [%s]"
																				(projectile-project-name))))
                (propertized-buffer-identification "%b")))
						 20 'down)))
	(put 'pv-mode-line-buffer-identification 'risky-local-variable t)
	(make-variable-buffer-local 'pv-mode-line-buffer-identification)

	(defun pv-replace-mode-line-buffer-identification (&optional reverse)
		(interactive "P")
		(moody-replace-element 'mode-line-buffer-identification
													 'pv-mode-line-buffer-identification
													 reverse)
		(moody-replace-element 'mode-line-misc-info
													 'pv-mode-line-mode
													 reverse))
	:config
  (setq x-underline-at-descent-line t)
	(setq moody-mode-line-height pvik-modeline-height) ;; defined in private.el
  (pv-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

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

;; =======================================================
;; =======================================================

;; tabs, spaces & indents
;; ======================

;; Enable tabs and set prefered indentation width in spaces
;; (In this case the indent size is 2-spaces wide)
(setq-default indent-tabs-mode t)
(setq-default standard-indent 2)
(setq-default tab-width 2)

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
	:ensure t)

;; Restclient
(use-package restclient
	:ensure t)
(use-package company-restclient
	:ensure t)

;; plantuml mode
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
  :ensure t)
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
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; org-mode
(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  (setq
   pvik--org-directory "~/SeaDrive/My Library/org"
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
  (global-set-key (kbd "C-c o o p") #'pvik--org-open-project-notes))
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
											:after-auth "#clojure" "#java")))))
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
 '(custom-safe-themes
	 (quote
		("b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(package-selected-packages
	 (quote
		(moody flycheck-nim nim-mode ac-geiser geiser flycheck-rust window-purpose w3m fill-column-indicator circe org spaceline-config eyebrowse helm-purpose scad-preview scad-mode spaceline neotree projectile which-key helm doom-themes use-package))))


;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-face ((t (:background "#3e4147" :foreground "#bbc2cf"))))
 '(ahs-plugin-defalt-face ((t (:background "#8795af" :foreground "Black")))))
