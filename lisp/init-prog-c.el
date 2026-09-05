;;; init-prog-c.el --- C/C++ programming configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ development environment with tree-sitter syntax highlighting,
;; eglot LSP support via clangd, and flymake diagnostics.

;;; Code:

;; c-ts-mode: Tree-sitter based C mode with enhanced syntax highlighting
;; and structural editing.
(use-feature c-ts-mode
  :defer t

  :init
  ;; Install tree-sitter grammars for C and C++ if not available.
  (my/ensure-treesit-grammar
   'c "https://github.com/tree-sitter/tree-sitter-c")
  (my/ensure-treesit-grammar
   'cpp "https://github.com/tree-sitter/tree-sitter-cpp")
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

  :custom
  ;; Indent by two spaces in K&R style.  Emacs derives neither from a
  ;; project's .clang-format, so projects that differ need a .dir-locals.el
  ;; entry.
  (c-ts-mode-indent-style 'k&r)
  (c-ts-mode-indent-offset 2)

  :bind
  ;; Switch between a source file and its header.
  (:map c-ts-base-mode-map
   ("C-c C-a" . ff-find-other-file)))

;; eglot: Configure C/C++-specific LSP settings with clangd.
;; Note: flymake is not enabled explicitly here -- eglot turns it on once
;; connected, and c-ts-mode brings no diagnostic backend of its own.
(use-feature eglot
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure))

(provide 'init-prog-c)

;;; init-prog-c.el ends here
