;;; programming.el --- Define and configure packages for programming modes

;;; Commentary:
;; Configuration for all Programming languages used

;;; Code:
;; Language Modes and config
;; =========================

(use-package aggressive-indent
  :ensure t)

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

;;;
;; lsp mode (go / elixir)
;;;

(use-package lsp-mode
  :ensure t
  :defer t
  :diminish lsp-mode
  :commands (lsp lsp-deferred)
  ;; (elixir-mode . lsp-deferred)
  :init
  ;; (add-to-list 'exec-path "/home/elric/Downloads/elixir-ls")
  :config
  ;; (setq lsp-clients-elixir-server-executable "elixir-ls")
  ;;  (lsp-register-custom-settings
  ;; '(("gopls.completeUnimported" t t)
  ;;   ("gopls.staticcheck" t t)))
  )

;; go mode

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  :custom
  ;; (setq lsp-ui-doc-delay 1)
  ;; (setq lsp-ui-sideline-delay 2)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-use-childframe t)
  ;; (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-max-height 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "⟩")
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-diagnostic-max-line-length 150)
  (lsp-ui-sideline-delay 1)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  :config
  (fci-mode 0)
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
	(interactive)
	(if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
	  (lsp-ui-doc-mode 1)))
  :bind
  ("C-c m"   . lsp-ui-imenu)
  ("C-c d"   . ladicle/toggle-lsp-ui-doc))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  :preface
  (defun my-go-mode-hook ()
	(go-projectile-set-gopath)
	;; (local-set-key (kbd "M-.") 'godef-jump)
	;; (local-set-key (kbd "M-*") 'pop-tag-mark)
	)  
  :config
  ;; (add-hook 'go-mode-hook 'lsp-deferred)
  (add-to-list 'exec-path "/home/elric/Work/gocode/bin")
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  ;; (local-set-key (kbd "M-.") 'godef-jump)
  ;; (local-set-key (kbd "M-*") 'pop-tag-mark)
  (add-hook 'go-mode-hook 'my-go-mode-hook))
(use-package go-playground
  :ensure t)
(use-package go-projectile
  :ensure t)

;; Rust Mode

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :bind
  (("C-c C-k". compile))
  :config
  (setq rust-format-on-save t)
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src") ;; Rust source code PATH

  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))
(use-package cargo
  :ensure t
  :defer t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package flycheck-rust
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'rust-mode
	(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
(use-package racer
  :ensure t
  :defer t)

;;;
;; lisp-y
;;;

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package smartparens
  :ensure t
  ;;:defer t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
		 (emacs-lisp-mode . show-paren-mode)
		 (emacs-lisp-mode . turn-on-smartparens-strict-mode)
		 (emacs-lisp-mode . outline-minor-mode))
  :init
  (require 'smartparens-init)
  :config  
  (smartparens-global-mode t))


;; common lisp
(add-hook 'after-init-hook
		  (lambda ()
			(load (expand-file-name "~/.quicklisp/slime-helper.el"))))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")
(use-package slime
  :ensure t
  :bind ("C-c C-z" . slime)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-fancy slime-banner slime-quicklisp slime-asdf))
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


(provide 'programming)

;; clojure
(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'show-paren-mode)
  (add-hook 'clojure-mode-hook #'projectile-mode)
  (add-hook 'clojure-mode-hook #'hl-todo-mode)
  (add-hook 'clojure-mode-hook #'outline-minor-mode))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook #'aggressive-indent-mode)
  (add-hook 'cider-mode-hook #'show-paren-mode)
  (add-hook 'cider-mode-hook #'outline-minor-mode))

;;; programming.el ends here
