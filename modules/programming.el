;;; programming.el --- Define and configure packages for programming modes

;;; Commentary:
;; Configuration for all Programming languages used

;;; Code:

;; display line numbers mode
;; (global-display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Language Modes and config
;; =========================

(use-package aggressive-indent
  :ensure t)

;; tree sitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
	 (heex "https://github.com/phoenixframework/tree-sitter-heex")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (use-package tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; company mode - complete anything
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq lsp-completion-provider :capf)
  (setq company-minimum-prefix-length 1
		company-idle-delay 0.0)) ;; default is 0.2
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

;;;
;; lsp mode
;;;

(use-package lsp-mode
  :ensure t
  :defer t
  :diminish lsp-mode
  :commands (lsp lsp-deferred)
  (elixir-mode . lsp-deferred)
  (elixir-ts-mode . lsp-deferred)
  :init
  (add-to-list 'exec-path "/usr/lib/elixir-ls")
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; rust
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-on-save t)
  (setq lsp-rust-server 'rust-analyzer)
  ;; (setq lsp-clients-elixir-server-executable "elixir-ls")
  ;;  (lsp-register-custom-settings
  ;; '(("gopls.completeUnimported" t t)
  ;;   ("gopls.staticcheck" t t)))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  ;; (setq lsp-ui-doc-delay 1)
  ;; (setq lsp-ui-sideline-delay 2)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  ;; (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'bottom)
  ;; (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-max-height 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "⟩")
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-diagnostic-max-line-length 150)
  (lsp-ui-sideline-delay 1)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  ;; (lsp-ui-peek-always-show t)
  ;; (lsp-ui-peek-enable t)
  ;; (lsp-ui-peek-peek-height 20)
  ;; (lsp-ui-peek-list-width 50)
  ;; (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  :config
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-lens-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
  (setq lsp-signature-render-documentation t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  ;;(fci-mode 0)
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
;; NOTE company-lsp is deprecated
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   :config
;;   (push 'company-lsp company-backends))

;; go mode
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  :preface
  (defun my-go-mode-hook ()
	(go-projectile-set-gopath)
	;; (local-set-key (kbd "M-*") 'pop-tag-mark)
	)
  (defun lsp-go-install-save-hooks ()
	"Set up before-save hooks to format buffer and add/delete imports.
     Make sure you don't have other gofmt/goimports hooks enabled."
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	(add-hook 'before-save-hook #'lsp-organize-imports t t))
  :config
  ;; (add-hook 'go-mode-hook 'lsp-deferred)
  (add-to-list 'exec-path "/home/elric/Work/gocode/bin")
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  ;; (local-set-key (kbd "M-.") 'godef-jump)
  ;; (local-set-key (kbd "M-*") 'pop-tag-mark)
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks))
(use-package go-playground
  :ensure t)
(use-package go-projectile
  :ensure t)

;; C/C++
;; make sure ccls package is installed on host OS
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable "/usr/bin/ccls"))

;; Rust Mode

;; (use-package rust-mode
;;   :ensure t
;;   :mode "\\.rs\\'"
;;   :after (lsp-mode)
;;   :hook
;;   (rust-mode . lsp-deferred)
;;   :bind
;;   (("C-c C-k". compile))
;;   :config
;;   (setq rust-format-on-save t)
;;   (setq lsp-rust-server 'rust-analyzer)
;;   (add-hook 'rust-mode-hook #'lsp)
;;   ;; (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
;;   ;; (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src") ;; Rust source code PATH
;;   ;; (add-hook 'rust-mode-hook #'racer-mode)
;;   ;; (add-hook 'racer-mode-hook #'eldoc-mode)
;;   ;; (add-hook 'racer-mode-hook #'company-mode)
;;   )
(use-package cargo
  :ensure t
  :defer t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package flycheck-rust
  :ensure t
  :defer t
  :after (flycheck rust-mode)
  :config
  (with-eval-after-load 'rust-mode
	(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
;; (use-package rustic
;;   :ensure t
;;   :defer t
;;   :after (rust-mode lsp-mode))
;; (use-package racer
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rustic
  :ensure
  ;; :mode "\\.rs\\'"
  ;; :after (lsp-mode)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom 
  (rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer"))
  :config
  (setq rustic-lsp-client 'lsp-mode)
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

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

;; erlang mode

;; (use-package ivy-erlang-complete
;;   :ensure t)

(use-package delight
  :ensure t)

(use-package erlang
  :load-path ("/usr/lib/erlang/lib/tools-3.5.3/emacs")
  ;; :hook (after-save . ivy-erlang-complete-reparse)
  ;; :custom (ivy-erlang-complete-erlang-root "<PATH TO OTP>/lib/erlang/")
  :mode (("\\.erl?$" . erlang-mode)
	 ("rebar\\.config$" . erlang-mode)
	 ("relx\\.config$" . erlang-mode)
	 ("sys\\.config\\.src$" . erlang-mode)
	 ("sys\\.config$" . erlang-mode)
	 ("\\.config\\.src?$" . erlang-mode)
	 ("\\.config\\.script?$" . erlang-mode)
	 ("\\.hrl?$" . erlang-mode)
	 ("\\.app?$" . erlang-mode)
	 ("\\.app.src?$" . erlang-mode)
	 ("\\Emakefile" . erlang-mode))
  :config
  ;; (ivy-erlang-complete-init)
  (setq erlang-root-dir "/usr/lib/erlang/")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (setq erlang-man-root-dir "/usr/lib/erlang/man"))

;; (setq load-path (cons "/usr/lib/erlang/lib/tools-3.5.3/emacs" load-path))
;; (require 'erlang-start)

;; elixir mode
;; (use-package elixir-mode
;;   :ensure t
;;   :hook
;;   (elixir-mode . lsp-deferred)
;;   :preface
;;   (defun elixir-save-hooks ()
;; 	"Set up before-save hooks to formatbuffer."
;; 	(add-hook 'before-save-hook 'elixir-format nil t))
;;   :config
;;   (add-hook 'elixir-mode-hook 'elixir-save-hooks))

;; eglot
;; (use-package eglot
;;  :ensure nil
;;  :config
;;  (add-to-list 'eglot-server-programs '(elixir-ts-mode "/usr/lib/elixir-ls/language_server.sh"))
;;  (add-to-list 'eglot-server-programs '(elixir-mode "/usr/lib/elixir-ls/language_server.sh")))

;; elixir mode
(use-package elixir-ts-mode
  :ensure t
  :hook
  ;; (elixir-mode . eglot-ensure)
  ;; (elixir-ts-mode . eglot-ensure)
  (elixir-mode . lsp-deferred)
  (elixir-ts-mode . lsp-deferred)
  :preface
  (defun elixir-save-hooks ()
	"Set up before-save hooks to formatbuffer."
	(add-hook 'before-save-hook 'elixir-format nil t))
  :config
  (add-hook 'elixir-mode-hook 'elixir-save-hooks)
  (major-mode-remap-alist
  '((elixir-mode . elixir-ts-mode))))

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
(use-package flycheck-clj-kondo
  :ensure t)
(use-package clojure-mode
  :ensure t
  :after (smartparens rainbow-delimiters flycheck-clj-kondo cider)
  :config
  (setq clojure-align-forms-automatically t)
  ;;:init
  ;;(add-to-list 'company-etags-mode 'clojure-mode)
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
								 ;; (pdf-occur-global-minor-mode -1)
								 ;; (tex-pdf-mode -1)
								 ))
  (add-hook 'cider-repl-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook (lambda ()
									(helm-cider-mode 1)
									;; (pdf-occur-global-minor-mode -1)
									;; (tex-pdf-mode -1)
									))
  (require 'flycheck-clj-kondo)
  )


;; Janet
(use-package janet-mode
  :ensure t
  :after (smartparens rainbow-delimiters aggressive-indent projectile)
  :config
  ;;:init
  (add-hook 'janet-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'janet-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'janet-mode-hook #'aggressive-indent-mode)
  (add-hook 'janet-mode-hook #'show-paren-mode)
  (add-hook 'janet-mode-hook #'projectile-mode)
  (add-hook 'janet-mode-hook #'hl-todo-mode)
  (add-hook 'janet-mode-hook #'outline-minor-mode))

(straight-use-package
 '(ijanet
   :type git
   :host github
   :repo "serialdev/ijanet-mode"
   ))

(provide 'programming)

;;; programming.el ends here
