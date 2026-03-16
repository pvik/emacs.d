;;; llm.el --- llm specific code  -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM Specific code

;;; Code:

(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key pvik-openai-token)
  ;; OpenRouter offers an OpenAI compatible API
  (gptel-make-openai "OpenRouter"               ;Any name you want
	:host "openrouter.ai"
	:endpoint "/api/v1/chat/completions"
	:stream t
	:key pvik-openrouter-token                  ;can be a function that returns the key
	:models '(openai/gpt-3.5-turbo
			  anthropic/claude-sonnet-4.6
			  anthropic/claude-haiku-4.5
			  moonshotai/kimi-k2.5
			  qwen/qwen3.5-9b
			  qwen/qwen3.5-122b-a10b
              mistralai/mixtral-8x7b-instruct
              meta-llama/codellama-34b-instruct
              codellama/codellama-70b-instruct
              google/palm-2-codechat-bison-32k
              google/gemini-pro))
  ;; (gptel-make-anthropic "Claude"
  ;; 						:stream t
  ;; 						:key pvik-anthropic-token)
  )

(provide 'llm)

;;; llm.el ends here
