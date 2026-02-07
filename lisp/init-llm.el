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

;; Monet: Visual IDE integration layer for Claude Code.  Provides inline diffs,
;; context highlighting, and other visual enhancements for AI-assisted coding.
(use-package monet
  :ensure (:host github :repo "stevemolitor/monet")

  :init
  (defun my/-monet-ediff-tool (old-file-path new-file-path new-file-contents
                               on-accept on-quit &optional session)
    "Wrapper around `monet-ediff-tool' that handles popper windows.
When called from a popper popup window, switches to a regular window first
to ensure ediff layout works correctly.

Returns the diff context object."
    (let ((original-window (selected-window)))
      ;; Ediff creates a complex window layout that conflicts with popper's
      ;; popup windows.  If we're in a popup, switch to a regular window
      ;; first to avoid layout issues.
      (when (and (fboundp 'popper-popup-p)
                 (popper-popup-p (current-buffer)))
        (let ((non-popup-window
               (cl-find-if (lambda (win)
                             (not (popper-popup-p (window-buffer win))))
                           (window-list (selected-frame)))))
          (when non-popup-window
            (select-window non-popup-window))))
      ;; Call the original ediff tool and store the original window in the
      ;; context so the cleanup function can restore it.
      (let ((context (monet-ediff-tool old-file-path new-file-path new-file-contents
                                       on-accept on-quit session)))
        (setf (alist-get 'original-window context) original-window)
        context)))

  (defun my/-monet-ediff-cleanup-tool (context)
    "Wrapper around `monet-ediff-cleanup-tool' that restores the original window.
Calls `monet-ediff-cleanup-tool' with CONTEXT, then switches back to the
original window if the current window differs from it."
    (monet-ediff-cleanup-tool context)
    (let ((original-window (alist-get 'original-window context)))
      (when (and original-window
                 (window-live-p original-window)
                 (not (eq (selected-window) original-window)))
        (select-window original-window))))

  :custom
  ;; Customize the diff tool to use ediff via wrappers that handle popper
  ;; windows.
  (monet-diff-tool #'my/-monet-ediff-tool)
  (monet-diff-cleanup-tool #'my/-monet-ediff-cleanup-tool)

  :config
  (monet-mode 1))

;; Claude Code: IDE integration for Claude AI assistant.  Enables interactive
;; coding sessions with Claude directly in Emacs.
(use-package claude-code
  :ensure
  (:host github
   :repo "stevemolitor/claude-code.el"
   :files (:defaults "examples/hooks/claude-code-auto-revert-hook.el"))

  :init
  ;; Declare external variable from eat package.
  (defvar eat-term-name)

  (defun my/-claude-code (&optional arg)
    "Start Claude Code with proper terminal emulation settings.

With prefix ARG, prompt for directory to run Claude Code in.

This wrapper sets `eat-term-name' to \"xterm-256color\" before starting
Claude Code.  This is necessary because the `claude-code-term-name'
variable doesn't work as expected --- its value is set too late in the
Claude Code Emacs package to properly affect the $TERM environment
variable in eat.  By setting `eat-term-name' here, we ensure the
terminal is properly configured before the eat process starts."
    (interactive "P")
    (let ((eat-term-name "xterm-256color"))
      (claude-code arg)))

  (defun my/-claude-code-display-buffer (buffer)
    "Display Claude Code BUFFER in a side window.
If there are multiple windows, display in another window.
If there's only one window, split and display on the right."
    (display-buffer
     buffer
     (if (= (count-windows) 1)
         ;; Only one window --- split horizontally and display on the right.
         '((display-buffer-in-direction)
           (direction . right)
           (window-width . 0.5))
       ;; Multiple windows --- use another window.
       '((display-buffer-use-some-window)
         (inhibit-same-window . t)))))

  :custom
  ;; Use custom display function to control Claude Code buffer placement.
  (claude-code-display-window-fn #'my/-claude-code-display-buffer)

  :config
  ;; Enable auto-revert for files modified by Claude.  The :git-merge option
  ;; uses git-merge to intelligently merge Claude's changes with any unsaved
  ;; local modifications, preventing data loss when both you and Claude edit
  ;; the same file.
  ;;
  ;; NOTE: For hooks to work, you need to:
  ;; 1. Add claude-code's "bin" directory to PATH for the
  ;;    `claude-code-hook-wrapper' script
  ;; 2. Configure ~/.claude/settings.json with hook commands
  ;;
  ;; See: https://github.com/stevemolitor/claude-code.el?tab=readme-ov-file#claude-code-hooks-integration
  ;;
  (require 'claude-code-auto-revert-hook)
  (setup-claude-auto-revert :git-merge t)

  ;; Fixing spaces between vertical bars.
  (add-hook 'claude-code-start-hook
            (lambda ()
              ;; Reduce line spacing to fix vertical bar gaps
              (setq-local line-spacing 0.1)))

  ;; Optional IDE integration with Monet.
  (when (featurep 'monet)
    (add-hook 'claude-code-process-environment-functions #'monet-start-server-function))

  (claude-code-mode)

  ;; Bind `C-c c' prefix to the Claude Code command map, providing quick access
  ;; to all Claude Code commands.
  :bind-keymap
  ("C-c c" . claude-code-command-map)

  :bind
  (:map claude-code-command-map
   ;; Use the wrapper to set `eat-term-name' before Claude Code starts.
   ("c" . my/-claude-code)
   ;; Define a repeat map so pressing `M' after invoking
   ;; `claude-code-cycle-mode' via `C-c c M' continues to cycle through
   ;; auto-accept/plan/confirm modes without needing the prefix.
   :repeat-map my-claude-code-map
   ("M" . claude-code-cycle-mode)))

(provide 'init-llm)

;;; init-llm.el ends here
