;;; llm.el --- llm specific code  -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM Specific code

;;; Code:

(use-package gptel
  :ensure t
  :config
  ;; (setq gptel-api-key pvik-openai-token)
  ;; OpenRouter offers an OpenAI compatible API
  (setq gptel-model   'gemma-4-31b-it
		gptel-backend
		(gptel-make-openai "OpenRouter"               ;Any name you want
		  :host "openrouter.ai"
		  :endpoint "/api/v1/chat/completions"
		  :stream t
		  :key pvik-openrouter-token                  ;can be a function that returns the key
		  :models '(anthropic/claude-opus-4.7
					anthropic/claude-sonnet-4.6
					anthropic/claude-haiku-4.5
					openai/gpt-5.4
					z-ai/glm-5.1
					google/gemma-4-31b-it
					google/gemini-3.1-pro-preview
					moonshotai/kimi-k2.5)))
  ;; (gptel-make-anthropic "Claude"
  ;; 						:stream t
  ;; 						:key pvik-anthropic-token)
  )

(provide 'llm)

;;; llm.el ends here
