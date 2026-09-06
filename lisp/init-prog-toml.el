;;; init-prog-toml.el --- TOML configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; TOML support with tree-sitter syntax highlighting.  Used for Cargo.toml,
;; pyproject.toml, lock files, and other TOML configuration files.

;;; Code:

;; toml-ts-mode: Built-in tree-sitter based TOML mode.
(use-feature toml-ts-mode
  :mode ("\\.toml\\'"
         ;; Lock files written in TOML that carry no `.toml' extension.
         "\\(?:\\`\\|/\\)\\(?:Cargo\\|uv\\)\\.lock\\'")

  :init
  ;; Install tree-sitter grammar for TOML if not available.
  (my/ensure-treesit-grammar
   'toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))

(provide 'init-prog-toml)

;;; init-prog-toml.el ends here
