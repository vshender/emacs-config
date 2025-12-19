;;; init-prog-yaml.el --- YAML configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; YAML support with LSP integration and structural editing.

;;; Code:

;; yaml-mode: Major mode for editing YAML files.
(use-package yaml-mode
  :mode ("\\.ya?ml\\'")

  :hook
  ((yaml-mode . eglot-ensure)))

;; eglot: Configure YAML-specific LSP settings.
(use-feature eglot
  :config
  (add-to-list 'eglot-server-programs
               '(yaml-mode . ("yaml-language-server" "--stdio"))))

;; yaml-pro: Structural editing and path display for YAML.
(use-package yaml-pro
  :after yaml-mode

  :init
  (my/ensure-treesit-grammar
   'yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")

  :hook
  ((yaml-mode . yaml-pro-ts-mode)))

(provide 'init-prog-yaml)

;;; init-prog-yaml.el ends here
