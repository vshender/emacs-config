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

(provide 'init-prog)

;;; init-prog.el ends here
