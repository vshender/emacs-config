;;; init-prog-css.el --- CSS configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; CSS editing configuration with tree-sitter and LSP support.

;;; Code:

;; css-mode: Built-in major mode for editing CSS stylesheets.
(use-feature css-mode
  :custom
  (css-indent-offset 2)

  :init
  ;; Install tree-sitter grammar for CSS if not available.
  (my/ensure-treesit-grammar
   'css "https://github.com/tree-sitter/tree-sitter-css")
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))

;; eglot: Configure CSS-specific LSP settings.  `css-base-mode' also covers
;; SCSS and LESS buffers, so the language server is used there as well.
(use-feature eglot
  :hook (css-base-mode . eglot-ensure)

  :config
  (add-to-list 'eglot-server-programs
               '((css-mode css-ts-mode scss-mode less-css-mode)
                 . ("vscode-css-languageserver" "--stdio"))))

(provide 'init-prog-css)

;;; init-prog-css.el ends here
