;;; init-markdown.el --- Markdown configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Markdown support with live preview and table of contents.

;;; Code:

;; markdown-mode: Major mode for Markdown files.
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :custom
  ;; Use pandoc for export and preview.
  (markdown-command "pandoc")
  ;; Use native syntax highlighting in fenced code blocks.
  (markdown-fontify-code-blocks-natively t)
  ;; Use variable-height fonts for headers.
  (markdown-header-scaling t)

  :bind
  (:map markdown-mode-map
   ("C-c C-e" . markdown-export)
   ("C-c C-v" . markdown-preview)))

;; markdown-toc: Generate table of contents for Markdown files.
(use-package markdown-toc
  :after markdown-mode

  :bind
  (:map markdown-mode-map
   ("C-c C-t" . markdown-toc-generate-or-refresh-toc)))

(provide 'init-markdown)

;;; init-markdown.el ends here
