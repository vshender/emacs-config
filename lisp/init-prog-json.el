;;; init-prog-json.el --- JSON configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; JSON support with tree-sitter, LSP, JSON path display, and jq integration.

;;; Code:

;; json-ts-mode: Built-in tree-sitter based JSON mode.
(use-feature json-ts-mode
  :mode ("\\.json\\'" "\\.jsonc\\'")

  :init
  (my/ensure-treesit-grammar
   'json "https://github.com/tree-sitter/tree-sitter-json"))

;; json: Built-in JSON parsing and formatting functions.
(use-feature json
  :after json-ts-mode

  :bind
  (:map json-ts-mode-map
   ("C-c C-f" . json-pretty-print-buffer)
   ("C-c C-r" . json-pretty-print)))

;; eglot: Configure JSON-specific LSP settings.
(use-feature eglot
  :hook (json-ts-mode . eglot-ensure)

  :config
  (add-to-list 'eglot-server-programs
               '(json-ts-mode . ("vscode-json-languageserver" "--stdio"))))

;; which-func: Display current function name in mode line for JSON paths.
(use-feature which-func
  :hook (json-ts-mode . which-function-mode))

;; json-snatcher: Copy path to JSON element at point.
(use-package json-snatcher
  :after json-ts-mode

  :bind
  (:map json-ts-mode-map
   ("C-c C-p" . jsons-print-path)))

;; jq-mode: Major mode for jq scripts and jq integration for JSON buffers.
(use-package jq-mode
  :after json-ts-mode

  :mode "\\.jq\\'"

  :bind
  (:map json-ts-mode-map
   ("C-c C-q" . jq-interactively)))

(provide 'init-prog-json)

;;; init-prog-json.el ends here
