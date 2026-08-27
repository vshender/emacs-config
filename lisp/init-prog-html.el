;;; init-prog-html.el --- HTML configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; HTML editing configuration with tree-sitter and LSP support.

;;; Code:

;; html-ts-mode: Built-in tree-sitter based HTML mode.
(use-feature html-ts-mode
  :init
  ;; Install tree-sitter grammar for HTML if not available.
  (my/ensure-treesit-grammar
   'html "https://github.com/tree-sitter/tree-sitter-html")
  (add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode)))

;; corfu: Enable the completion popup.  `html-ts-mode' derives from
;; `text-mode', not `prog-mode', so the corfu hooks from init-completion
;; don't cover it.
(use-feature corfu
  :hook (html-ts-mode . corfu-mode))

;; eglot: Configure HTML-specific LSP settings.
(use-feature eglot
  :hook (html-ts-mode . eglot-ensure)

  :config
  (add-to-list 'eglot-server-programs
               '(html-ts-mode . ("vscode-html-languageserver" "--stdio"))))

(provide 'init-prog-html)

;;; init-prog-html.el ends here
