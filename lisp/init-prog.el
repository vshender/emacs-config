;;; init-prog.el --- Programming mode configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Common settings for programming modes including line numbers, syntax
;; checking, project management, tree-sitter, and LSP integration.

;;; Code:

;; display-line-numbers: Built-in feature to show line numbers in the margin.
(use-feature display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

;; ediff: Built-in visual diff and merge tool.
(use-feature ediff
  :defer t

  :custom
  ;; Use a single frame instead of spawning a separate control frame.
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Show diffs side by side.
  (ediff-split-window-function #'split-window-horizontally))

;; flymake: Built-in on-the-fly syntax checking.
(use-feature flymake
  :bind
  (:map flymake-mode-map
   ("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

;; project: Built-in project management for Emacs.
(use-feature project
  :custom
  ;; Store project list in var/ directory.
  (project-list-file (expand-file-name "projects" my/var-dir))

  :init
  (defun my/project-root ()
    "Return the root directory of the current project.
Prompt the user for the project to use if no project is found."
    (let ((project (project-current t)))
      (project-root project)))

  (defun my/project-ripgrep ()
    "Search the current project with ripgrep via Consult."
    (interactive)
    (consult-ripgrep (my/project-root)))

  :config
  ;; Display the current project in the mode line if supported (Emacs 30+).
  (when (and (>= emacs-major-version 30)
             (boundp 'project-mode-line))
    (setopt project-mode-line t))

  :bind
  (([remap project-find-regexp] . my/project-ripgrep)))

;; treesit: Built-in tree-sitter support for enhanced syntax highlighting
;; and structural editing.
(use-feature treesit
  :config
  (defconst my/treesit-install-dir (expand-file-name "tree-sitter" my/var-dir)
    "Directory for storing tree-sitter grammar libraries.")

  ;; Tell treesit to look for grammars in our custom directory.
  (setq treesit-extra-load-path (list my/treesit-install-dir))

  (defun my/ensure-treesit-grammar (lang url)
    "Ensure tree-sitter grammar for LANG is installed from URL.
If the grammar is not available, temporarily set `treesit-language-source-alist'
and install it automatically."
    (unless (treesit-language-available-p lang)
      (let ((treesit-language-source-alist `((,lang ,url))))
        (treesit-install-language-grammar lang my/treesit-install-dir)))))

;; eglot: Built-in LSP client for language server integration.  Provides
;; code completion, diagnostics, and refactoring support.
(use-feature eglot
  :defer t

  :custom
  ;; Shutdown language server when last managed buffer is killed.
  (eglot-autoshutdown t)
  ;; Disable events buffer for performance.
  (eglot-events-buffer-size 0))

(provide 'init-prog)

;;; init-prog.el ends here
