;;; llm.el --- llm specific code  -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM Specific code

;;; Code:

(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key pvik-openai-token)
  (gptel-make-anthropic "Claude"
						:stream t
						:key pvik-anthropic-token))

(provide 'llm)

;;; llm.el ends here
