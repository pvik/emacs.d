;;; init.el --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Emacs init file

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

;; Add my library path to load-path
(push "~/.emacs.d/modules/" load-path)
(setq load-prefer-newer t) ;; load newer source instead of old bytecode
;;(add-to-list 'load-path "~/.emacs.d/modules/")
(require 'private)
;;; (require 'doom-core-slim)

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(setq-default
 inhibit-startup-screen t
 initial-scratch-message ";;-*- lexical-binding: t -*-\n\n;; Happy Hacking!!\n\n;; C-c C-f C-s : Split Windows\n;; C-c C-f C-n : Next Window\n;; C-c C-f C-p : Previous Window\n;; C-c C-f C-f : Move Buffer to Next Window\n;; C-c C-f C-b : Move Buffer to Previous Window\n;; C-c C-f C-w : Swap Buffer with next window\n\n;; C-c o n : Open Notes\n;; C-c o w : Work Notes\n;; C-c o p : Project Notes\n\n;; -----\n"
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

(global-display-fill-column-indicator-mode 1)
(setq-default display-fill-column-indicator t)

;;;
;; use package
;;;

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(require 'use-package)

(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

;;;
;; straight.el
;;;

;; ;; Bootstrap straight.el
;; (defvar bootstrap-version)
;; (setq straight-repository-branch "develop")
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (setq package-enable-at-startup nil)

;; ;; Always use straight to install on systems other than Linux
;; (setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; ;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;; ;; Load the helper package for commands like `straight-x-clean-unused-repos'
;; (require 'straight-x)

;;;
;; Keep Emacs.d clean
;;;

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.emacs.d/cache/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :ensure t)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;;
;; Default use UTF-8
;;;

(set-default-coding-systems 'utf-8)

;;;
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

;;;
;; Packages
;;;

(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ;; ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/common-lisp/" "~/Work/"))
  ;; ignoring specific buffers by name
  (setq projectile-globally-ignored-buffers
		'("*scratch*"
		  "*lsp-log*"
		  "*gopls::stderr"))
  ;; ignoring buffers by their major mode
  (setq projectile-globally-ignored-modes
		'("erc-mode"
		  "help-mode"
		  "completion-list-mode"
		  "Buffer-menu-mode"
		  "gnus-.*-mode"
		  "occur-mode")))

;;;
;; Buffers
;;;

;; kill this buffer key bind
(global-set-key (kbd "C-x C-k .") (lambda ()
                              (interactive)
                              (kill-buffer (buffer-name))))

;; tabs, spaces & indents

;; Enable tabs and set prefered indentation width in spaces
;; (In this case the indent size is 2-spaces wide)
(setq-default indent-tabs-mode t)
(setq-default standard-indent 4)
(setq-default tab-width 4)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; linum mode for all files
;; (add-hook 'text-mode-hook 'linum-mode)
;; (add-hook 'prog-mode-hook 'linum-mode)

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

(dolist (buffer *protected-buffers*)
  (with-current-buffer buffer
    (emacs-lock-mode 'kill)))

;; (defun my/protected-buffers ()
;;   "Protects some buffers from being killed."
;;   (dolist (buffer *protected-buffers*)
;;     (with-current-buffer buffer
;;       (emacs-lock-mode 'kill))))

;; (add-hook 'after-init-hook #'my/protected-buffers)

(use-package unicode-fonts
  :ensure t
  ;;:defer t
  :config
  (unicode-fonts-setup))

;; All The Icons
(use-package all-the-icons
  :ensure t)

(use-package diminish
  :ensure t)

(column-number-mode 1)

(use-package doom-themes
  :ensure t
  :preface
  ;; (defun my-init-theme (&optional _frame)
  ;; 	)
  
  ;; (defun my-reload-theme-in-daemon (frame)
  ;; 	(when (or (daemonp) (not (display-graphic-p)))
  ;;     (with-selected-frame frame
  ;; 		(run-with-timer 0.1 nil #'my-init-theme))))
  :config
  (setq doom-themes-enable-bold nil  ; if nil, bold is universally disabled
		doom-themes-enable-italic t) ; if nil, italics is universally disabled
  
  (doom-themes-visual-bell-config) ;; Enable flashing mode-line on errors
  (doom-themes-neotree-config) ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-org-config) ;; Corrects (and improves) org-mode's native fontification.
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
  (diminish 'clj-refactor-mode)

  ;; (add-hook 'after-make-frame-functions #'my-init-theme)
  ;; (add-hook 'after-make-frame-functions #'my-reload-theme-in-daemon)
  
  ;; for GUI sessions
  ;; (my-init-theme)
  (set-face-attribute 'default nil :height pvik-default-font-height))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;;
;; Helm
;;;

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
  :bind (("M-x" . helm-M-x)
		 ("C-x b" . helm-mini)
		 ("C-x C-f" . helm-find-files)
         ("M-<f5>" . helm-find-files)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)
		 :map helm-map
		 ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
		 ("C-i"   . helm-execute-persistent-action) ; make TAB work in terminal
		 ("C-z"   . helm-select-action)) ; list actions using C-z
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
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
		 (rx "*lsp-")
		 (rx "*gopls*")
		 (rx "*Ibuffer*")
		 (rx "*helm")
		 (rx "*straight-process*")
         (rx " ")))
  ;; (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  ;; (helm-autoresize-mode t)
  :config
  ;; (global-unset-key (kbd "C-x b"))
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  ;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-c h o") #'helm-occur)
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
;; (use-package helm-tramp
;;   :ensure t
;;   :config
;;   (add-hook 'helm-tramp-pre-command-hook (lambda () (global-aggressive-indent-mode 0)
;; 					    (projectile-mode 0)))
;;   (add-hook 'helm-tramp-quit-hook (lambda () (global-aggressive-indent-mode 1)
;; 				     (projectile-mode 1))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; (use-package neotree
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c n") #'neotree-toggle))

(use-package treemacs
  :ensure t
  :defer t
  :init
  :config
  (treemacs-project-follow-mode)
  :bind
  (:map global-map
        ("C-c n t"       . treemacs-select-window)
        ;; ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-c n n"   . treemacs)
        ;; ("C-x t d"   . treemacs-select-directory)
        ;; ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ;; ("C-x t M-t" . treemacs-find-tag)
		))

(use-package treemacs-projectile
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("C-c n p"   . treemacs-projectile)))

;; org-mode
;; ========

(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook #'display-line-numbers-mode)
  ;; (setq org-directory (expand-file-name "~/org"))
  (setq org-default-notes-file (expand-file-name pvik-default-notes-file))
  ;; (setq org-agenda-files '("~/org" "~/www/org" "~/www/_org"))
  ;; hide leading stars
  (setq org-startup-indented t
		org-hide-leading-stars t)
  (setq org-adapt-indentation t)
  (setq org-startup-with-inline-images t)
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((d2 . t)
      (mermaid . t))))

(use-package ob-mermaid
  :ensure t
  :config
  (setq ob-mermaid-cli-path "/usr/bin/mmdc"))

(use-package ob-d2
  :ensure t
  :config
  (setq ob-d2-cli-path "/usr/bin/d2"))

;; magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; (setq vc-handled-backends (delq 'Git vc-handled-backends))
  )

;; Programming
;; ==========

(require 'programming)

;; Restclient
(use-package restclient
  :ensure t
  :defer t)
(use-package company-restclient
  :ensure t
  :defer t)
(use-package ob-restclient
  :ensure t
  :defer t)

(use-package htmlize
  :ensure t
  :defer t)

;; plantuml mode
(use-package plantuml-mode
  :ensure t
  :defer t
  :mode "\\.puml\\'"
  :config
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))
(use-package flycheck-plantuml
  :ensure t
  :defer t
  :after flycheck
  :config
  (with-eval-after-load 'flycheck
	(add-hook 'flycheck-mode-hook #'flycheck-plantuml-setup)))

;; d2 lang mode
(use-package d2-mode
  :ensure t
  :defer t
  :mode "\\.d2\\'"
  :config
  ;; (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  ;; (setq plantuml-default-exec-mode 'jar)
  )

;; Data modes
;; ==========

(use-package json-mode
  :mode "\\.json\\'")

;; (use-package nxml
;;   :ensure t)
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

;; CSV
(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'"
  :config
  (setq-default csv-align-padding 2))

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

;; flyspell (for text files)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(global-set-key (kbd "<f8>") 'ispell-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

;; eshell
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (setenv "TERM" "xterm-256color")))
;; (add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
;; (setq eshell-output-filter-functions ())
;; (setq eshell-output-filter-functions
;;       (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
;; (setq eshell-preoutput-filter-functions ())
;; (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

;; Desktop
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/cache/"))
(desktop-read)

;; Theme
(if (or (daemonp) (display-graphic-p))
	  (load-theme 'doom-one t) ;; GUI Theme
	  (load-theme 'doom-dracula t))

;; Default font
(setq default-font (if pvik-default-font pvik-default-font "SourceCodePro"))

;; fonts
(set-frame-font default-font)
(set-face-attribute 'default nil :family default-font :height pvik-default-font-height :weight 'normal)
(set-face-attribute 'mode-line          nil :height pvik-modeline-active-font-height) ;; defined in private.el
(set-face-attribute 'mode-line-inactive nil :height pvik-modeline-inactive-font-height)
(setq-default line-spacing pvik-line-spacing)

;; garbage collect on focus out
(add-hook 'focus-out-hook 'garbage-collect)

;; Emacs Personal Functions and Customization
;; ==========================================
(require 'pvik-core)

;; for key bindings
(pvik-keys-minor-mode 1)

;;;
;; Frames
;;;
(require 'frames)
;; (pvik/split-windows)

;;;
;; osx
;;;
(if pvik-is-osx
	(progn (require 'osx))

  ;; non-osx stuff
  (progn (setq alert-default-style 'libnotify)))

;;;
;; LLM
;;;
(require 'llm)

(use-package envrc
  :ensure t)
(envrc-global-mode)


(setq gc-cons-threshold gc-cons-threshold-orig)

;;; init.el ends here
