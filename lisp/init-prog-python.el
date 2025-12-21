;;; init-prog-python.el --- Python programming configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Python development environment with tree-sitter syntax highlighting,
;; eglot LSP support, and automatic virtual environment detection.

;;; Code:

;; python-ts-mode: Tree-sitter based Python mode with enhanced syntax
;; highlighting and structural editing.
(use-feature python
  :custom
  ;; Standard Python indentation.
  (python-indent-offset 4)
  ;; Don't add extra indentation for def/class blocks.
  (python-indent-def-block-scale 1)
  ;; Suppress "Can't guess indent offset" warnings.
  (python-indent-guess-indent-offset-verbose nil)

  :init
  ;; Install tree-sitter grammar for Python if not available.
  (my/ensure-treesit-grammar
   'python "https://github.com/tree-sitter/tree-sitter-python")
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  :hook
  ((python-base-mode . flymake-mode)
   (python-base-mode . eglot-ensure)))

;; eglot: Configure Python-specific LSP settings.
(use-feature eglot
  :config
  ;; Configure basedpyright as the preferred Python language server.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio"))))

;; pet: Python Executable Tracker.  Automatically detects and uses the correct
;; Python interpreter for virtual environments (uv, poetry, pipenv, venv, etc.).
(use-package pet
  :custom
  ;; Limit file search functions for better performance.
  (pet-find-file-functions '(pet-find-file-from-project-root
                             pet-locate-dominating-file))

  ;; Only search for executables within project virtualenvs, not globally.
  ;; When enabled, pet searches system-wide for each executable not found
  ;; locally, resulting in several-second Python file open time.
  (pet-search-globally nil)

  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; flymake-ruff: Ruff linter integration via Flymake.
(use-package flymake-ruff
  :hook
  ;; Run ruff only when Eglot is managing the buffer (keeps it "project aware").
  (eglot-managed-mode . (lambda ()
                          (when (derived-mode-p 'python-base-mode)
                            (when-let ((ruff (pet-executable-find "ruff")))
                              (setq-local flymake-ruff-program ruff)
                              (flymake-ruff-load))))))

;; python-docstring: Insert and format Python docstrings.
(use-package python-docstring
  :defer t

  :hook (python-base-mode . python-docstring-mode))

(provide 'init-prog-python)

;;; init-prog-python.el ends here
