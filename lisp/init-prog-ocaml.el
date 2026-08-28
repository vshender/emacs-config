;;; init-prog-ocaml.el --- OCaml programming configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; OCaml development environment with eglot LSP support (ocaml-lsp-server)
;; and REPL integration.

;;; Code:

;; tuareg: Major mode for editing OCaml code with comprehensive syntax support,
;; indentation, and integration with OCaml tools.
(use-package tuareg
  ;; The package autoloads already associate OCaml sources, Menhir and opam
  ;; files, and the `ocaml' interpreter with the right modes; only the
  ;; toplevel init file needs an explicit association.
  :mode ("\\.ocamlinit\\'" . tuareg-mode)

  :custom
  ;; Align continuation arguments with the first argument.
  (tuareg-indent-align-with-first-arg t)
  ;; Align the pipes of multiple patterns of a single case.
  (tuareg-match-patterns-aligned t))

;; ocaml-eglot: Minor mode overlay on eglot for OCaml-specific LSP features.
;; Provides error navigation, type information with adjustable verbosity,
;; and code generation (destruct, pattern match construction).  Eglot's
;; default `eglot-server-programs' already maps `tuareg-mode' to ocamllsp.
;; Requires ocaml-lsp-server: opam install ocaml-lsp-server
(use-package ocaml-eglot
  :hook
  ((tuareg-mode . ocaml-eglot-mode)
   (ocaml-eglot-mode . eglot-ensure)))

;; ocp-indent: Automatic indentation tool for OCaml code.
;; Requires ocp-indent: opam install ocp-indent
(use-package ocp-indent
  :hook
  (tuareg-mode . ocp-setup-indent)

  :init
  ;; ocp-indent.el calls `list*' from the deprecated cl.el library while only
  ;; requiring cl-lib, where the function is named `cl-list*'.  The call is only
  ;; reached when `ocp-indent-syntax' is non-nil, but would then fail with a
  ;; void-function error unless cl.el happens to be loaded.  Provide the missing
  ;; alias until it is fixed upstream:
  ;; https://github.com/OCamlPro/ocp-indent/blob/master/tools/ocp-indent.el
  (unless (fboundp 'list*)
    (defalias 'list* #'cl-list*)))

;; opam-switch-mode: Minor mode for switching between opam switches from Emacs.
;; Displays current switch in mode line and provides commands to change switches.
(use-package opam-switch-mode
  :hook
  (tuareg-mode . opam-switch-mode))

;; dune: Major mode for editing Dune build system files.
(use-package dune)

;; utop: Interactive OCaml toplevel with better features than the standard REPL.
;; Requires utop: opam install utop
(use-package utop
  :hook
  ;; Use utop minor mode in tuareg for easy REPL interaction.
  (tuareg-mode . utop-minor-mode)

  :custom
  ;; Use utop as the default OCaml REPL.
  (utop-command "opam exec -- utop -emacs")

  :bind
  (:map tuareg-mode-map
   ("C-c C-s" . utop)
   ("C-c C-e" . utop-eval-phrase)
   ("C-c C-r" . utop-eval-region)
   ("C-c C-b" . utop-eval-buffer)))

(provide 'init-prog-ocaml)

;;; init-prog-ocaml.el ends here
