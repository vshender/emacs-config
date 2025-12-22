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

;; eldoc: Built-in feature that displays documentation for the symbol at
;; point in the echo area or a dedicated buffer.
(use-feature eldoc
  :config
  ;; Some LSP servers return documentation with HTML entities (e.g., &lt;
  ;; instead of <).  This advice preprocesses eldoc output to replace these
  ;; entities with their actual characters.
  ;; See https://emacs.stackexchange.com/a/82952/9782
  (defvar my/-eldoc-html-patterns
    '(("&nbsp;" " ")
      ("&lt;" "<")
      ("&gt;" ">")
      ("&amp;" "&")
      ("&quot;" "\"")
      ("&apos;" "'"))
    "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

  (defun my/-string-replace-all (patterns in-string)
    "Replace all cars from PATTERNS in IN-STRING with their pair."
    (mapc (lambda (pattern-pair)
            (setq in-string
                  (string-replace (car pattern-pair)
                                  (cadr pattern-pair)
                                  in-string)))
          patterns)
    in-string)

  (defun my/-eldoc-preprocess (orig-fun &rest args)
    "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
    (let ((doc (car args)))
      ;; The first argument is a list of (STRING :KEY VALUE ...) entries
      ;; we replace the text in each such string
      ;; see docstring of `eldoc-display-functions'
      (when (listp doc)
        (setq doc
              (mapcar
               (lambda (doc)
                 (cons (my/-string-replace-all my/-eldoc-html-patterns (car doc))
                       (cdr doc)))
               doc)))
      (apply orig-fun (cons doc (cdr args)))))

  (advice-add 'eldoc-display-in-buffer :around #'my/-eldoc-preprocess))

(provide 'init-prog)

;;; init-prog.el ends here
