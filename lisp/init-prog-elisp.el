;;; init-prog-elisp.el --- Emacs Lisp programming configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Lisp programming support with enhanced editing, navigation, and debugging.

;;; Code:

;; elisp-mode: Built-in Emacs Lisp major mode.
(use-feature elisp-mode
  :bind
  (:map emacs-lisp-mode-map
   ("C-c C-c" . eval-defun)
   ("C-c C-b" . eval-buffer)
   ("C-c C-r" . eval-region)
   ("C-c C-p" . pp-eval-last-sexp)
   ("C-c C-m" . pp-macroexpand-last-sexp)
   :map lisp-interaction-mode-map
   ("C-c C-c" . eval-defun)
   ("C-c C-b" . eval-buffer)
   ("C-c C-r" . eval-region)))

;; flymake: Enable on-the-fly syntax checking for Emacs Lisp.
(use-feature flymake
  :hook (emacs-lisp-mode . flymake-mode))

;; eldoc: Show function signatures and variable documentation in echo area.
(use-feature eldoc
  :hook (emacs-lisp-mode . eldoc-mode)

  :custom
  ;; Show documentation quickly after stopping.
  (eldoc-idle-delay 0.1)
  ;; Allow multiline documentation in the echo area.
  (eldoc-echo-area-use-multiline-p t))

;; ielm: Interactive Emacs Lisp Mode --- a REPL for Emacs Lisp.
(use-feature ielm
  :defer t

  :custom
  ;; Store IELM history in the var/ directory.
  (ielm-history-file-name (expand-file-name "ielm-history.eld" my/var-dir)))

;; checkdoc: Check Emacs Lisp documentation conventions.
(use-feature checkdoc
  :defer t

  :custom
  ;; Disable spellchecking in docstrings.
  (checkdoc-spellcheck-documentation-flag nil))

;; edebug: Source-level debugger for Emacs Lisp.
(use-feature edebug
  :defer t

  :custom
  ;; Maximum list length to print before truncating.
  (edebug-print-length 100)
  ;; Maximum nesting depth to print before truncating.
  (edebug-print-level 10))

;; highlight-quoted: Highlight Lisp quotes and quoted symbols.
(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;; eros: Evaluation Result OverlayS --- show eval results inline.
(use-package eros
  :hook
  (emacs-lisp-mode . eros-mode))

;; ipretty: Interactive pretty-printing for Emacs Lisp evaluation.
(use-package ipretty
  :config
  (ipretty-mode 1))

;; macrostep: Interactive macro expansion for Emacs Lisp.
(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)))

(provide 'init-prog-elisp)

;;; init-prog-elisp.el ends here
