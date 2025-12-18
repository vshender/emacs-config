;;; init-llm.el --- LLM integration configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; LLM integrations for AI-assisted coding:
;; - gptel: Multi-backend LLM client (OpenAI, Claude, Gemini)
;; - MCP: Model Context Protocol servers for extended capabilities
;; - Monet: Visual IDE layer with inline diffs for Claude Code
;; - Claude Code: Terminal-based Claude AI integration via eat

;;; Code:

;; gptel: LLM client for Emacs supporting multiple backends (OpenAI, Anthropic,
;; Gemini, etc.).  Provides interactive chat interface and text generation.
(use-package gptel
  :custom
  (gptel-model 'gpt-5-mini)

  :config
  ;; Enable MCP integration for gptel.
  (require 'gptel-integrations)

  ;; Register the Anthropic and Gemini backends.
  (gptel-make-anthropic "Claude" :stream t)
  (gptel-make-gemini "Gemini" :stream t)

  :bind
  (("C-c <return>" . gptel-send)))

;; MCP: Model Context Protocol integration.  Provides access to various MCP
;; servers (filesystem, fetch, ripgrep, shell) for enhanced LLM capabilities.
(use-package mcp
  :custom
  (mcp-hub-servers
   `(("filesystem" . (:command "npx"
                      :args ("-y" "@modelcontextprotocol/server-filesystem")
                      :roots (,(getenv "HOME"))))
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ("ripgrep" . (:command "npx" :args ("-y" "mcp-ripgrep@latest")))
     ("shell" . (:command "uvx"
                 :args ("mcp-shell-server")
                 :env (:ALLOW_COMMANDS
                       "ls,cat,pwd,grep,wc,touch,find,cp,mv,echo")))))

  :config
  (require 'mcp-hub))

(provide 'init-llm)

;;; init-llm.el ends here
