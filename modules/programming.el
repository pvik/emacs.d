;;; programming.el --- Define and configure packages for programming modes

;;; Commentary:
;; Configuration for all Programming languages used

;;; Code:
;; Language Modes and config
;; =========================

;; company mode - complete anything
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-web
  :ensure t)
(use-package company-shell
  :ensure t)

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; folding
;; (use-package outline-magic
;;   :ensure t
;;   :config
;;   (add-hook 'outline-mode-hook
;;             (lambda ()
;;               (require 'outline-cycle)))
;;   (add-hook 'outline-minor-mode-hook
;;             (lambda ()
;;               (require 'outline-magic)
;;               (define-key outline-minor-mode-map  (kbd "<C-tab>") 'outline-cycle))))

;; hl-todo
(use-package hl-todo
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package auto-highlight-symbol
  :ensure t
  :custom-face
  (ahs-face ((t (:background "#3e4147" :foreground "#bbc2cf"))))
  (ahs-plugin-defalt-face ((t (:background "#8795af" :foreground "Black"))))
  :config
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)
  ;;(global-auto-highlight-symbol-mode t)
  )

(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fci-mode))

;; lisp-y

(use-package smartparens
  :ensure t
  :init
  (load "smartparens-init")
  :config
  (smartparens-global-mode t))

;; geiser -> chicken scheme
(use-package geiser
  :ensure t)
(use-package ac-geiser
  :ensure t)
(add-hook 'scheme-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'show-paren-mode)
(add-hook 'scheme-mode-hook #'outline-minor-mode)

;; (use-package paredit
;;   :ensure t
;;   :config
;;   (unbind-key "C-M-<left>" paredit-mode-map)
;;   (unbind-key "C-M-<right>" paredit-mode-map))

(use-package rainbow-delimiters
  :ensure t)
(use-package aggressive-indent
  :ensure t)

;; emacs-lisp-mode
;;(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

;; common lisp
(use-package slime
  :ensure t
  :config
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf))
  (add-hook 'slime-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'slime-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'slime-mode-hook #'aggressive-indent-mode)
  (add-hook 'slime-mode-hook #'show-paren-mode)
  (add-hook 'slime-mode-hook #'projectile-mode)
  (add-hook 'slime-mode-hook #'hl-todo-mode)
  (add-hook 'slime-mode-hook #'outline-minor-mode)
  ;; repl
  (add-hook 'slime-repl-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'slime-repl-mode-hook #'aggressive-indent-mode)
  (add-hook 'slime-repl-mode-hook #'show-paren-mode))
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")

;; clojure
(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook 'eldoc-mode)
  :config
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-log-messages t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-boot-parameters "repl -s watch refresh")
  (setq nrepl-repl-buffer-name-template "*cider-repl (%r%S)"))
(use-package helm-cider
  :ensure t
  :after cider)
;; (use-package flycheck-clojure
;;   :ensure t
;;   :after (flycheck cider)
;;   :init
;;   (flycheck-clojure-setup)
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   (add-hook 'cider-mode-hook
;; 	    (lambda () (setq next-error-function #'flycheck-next-error-function))))
(use-package clj-refactor
  :ensure t)
(use-package clojure-mode
  :ensure t
  :after (;paredit
		  smartparens rainbow-delimiters aggressive-indent cider flycheck-clojure clj-refactor projectile)
  :config
  (setq clojure-align-forms-automatically t)
  ;;:init
  ;;(add-to-list 'company-etags-mode 'clojure-mode)
  )

;;(add-hook 'clojure-mode-hook #'paredit-mode)
										;(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'show-paren-mode)
(add-hook 'clojure-mode-hook #'projectile-mode)
(add-hook 'clojure-mode-hook #'hl-todo-mode)
(add-hook 'clojure-mode-hook #'outline-minor-mode)
(add-hook 'clojure-mode-hook (lambda ()
							   (clj-refactor-mode 1)
							   (helm-cider-mode 1)
							   (yas-minor-mode 1) ; for adding require/use/import statements
							   (cljr-add-keybindings-with-prefix "M-RET")
							   (pdf-occur-global-minor-mode -1)
							   (tex-pdf-mode -1)))
(add-hook 'cider-repl-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook (lambda ()
								  (helm-cider-mode 1)
								  (pdf-occur-global-minor-mode -1)
								  (tex-pdf-mode -1)))

;; erlang mode

(setq load-path (cons "/usr/lib/erlang/lib/tools-3.3/emacs" load-path))
(require 'erlang-start)
(setq erlang-root-dir "/usr/lib/erlang/")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(setq erlang-man-root-dir "/usr/lib/erlang/man")

;; go mode
(use-package company-go
  :config
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                          ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  )
(use-package go-mode
  :ensure t
  :preface
  (defun my-go-mode-hook ()
	(go-projectile-set-gopath)
	(local-set-key (kbd "M-.") 'godef-jump)
	(local-set-key (kbd "M-*") 'pop-tag-mark))
  :config
  (add-to-list 'exec-path "/home/elric/Work/gocode/bin")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (add-hook 'go-mode-hook 'my-go-mode-hook))
(use-package go-playground
  :ensure t)
(use-package go-projectile
  :ensure t)

;; Rust Mode

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src") ;; Rust source code PATH

  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))
(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
	(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
;; (use-package flymake-rust
;;   :ensure t)
(use-package racer
  :ensure t)
(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; nim mode
(use-package nim-mode
  :ensure t
  :config
  (add-hook 'nim-mode-hook 'nimsuggest-mode))

;; sql mode
(use-package sql-indent
  :ensure t
  :after (:any sql sql-interactive-mode))

;;  tech modes
;; (use-package sgml-mode
;;   :ensure nil
;;   :hook
;;   ((html-mode . sgml-electric-tag-pair-mode)
;;    (html-mode . sgml-name-8bit-mode)
;;    (html-mode . toggle-truncate-lines))
;;   :config
;;   (setq-default sgml-basic-offset 2))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))

(add-hook 'web-mode-hook (lambda ()
						   (electric-indent-mode -1)
						   (pdf-occur-global-minor-mode -1)
						   (tex-pdf-mode -1)))

(use-package css-mode
  :ensure nil
  :config (setq-default css-indent-offset 2))

(use-package scss-mode
  :ensure nil
  :preface
  (defun me/scss-set-comment-style ()
    (setq-local comment-end "")
    (setq-local comment-start "//"))
  :delight scss-mode "SCSS"
  :mode ("\\.sass\\'" "\\.scss\\'")
  :hook (scss-mode . me/scss-set-comment-style))

(use-package less-css-mode
  :ensure t)
;; (use-package sass-mode
;;   :ensure t)
(use-package js2-mode
  :ensure t)
;; (use-package web-beautify
;;   :ensure t
;;   :config
;;   (eval-after-load 'js2-mode
;;     '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;;   (eval-after-load 'json-mode
;;     '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
;;   (eval-after-load 'sgml-mode
;;     '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
;;   (eval-after-load 'web-mode
;;     '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
;;   (eval-after-load 'css-mode
;;     '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css)))

;; scad-mode
(use-package scad-mode
  :ensure t)

;; flyspell (progn modes)
(dolist (mode '(emacs-lisp-mode-hook
                inferior-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js2-mode-hook
				sgml-mode-hook
				json-mode-hook))
  (add-hook mode
            '(lambda ()
               (flyspell-prog-mode))))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(provide 'programming)

;;; programming.el ends here
