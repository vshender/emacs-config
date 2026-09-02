;;; init-prog-rust.el --- Rust programming configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Rust development environment with tree-sitter syntax highlighting,
;; eglot LSP support (rust-analyzer), and cargo integration.

;;; Code:

;; rust-ts-mode: Built-in tree-sitter based Rust mode.
(use-feature rust-ts-mode
  :mode "\\.rs\\'"

  :init
  ;; Install tree-sitter grammar for Rust if not available.
  (my/ensure-treesit-grammar
   'rust "https://github.com/tree-sitter/tree-sitter-rust"))

;; project: Treat any directory containing Cargo.toml as a project root.
;; Rust-analyzer discovers the workspace starting from the project root and
;; doesn't search subdirectories, so when a Cargo project is nested inside a
;; larger repository, eglot would otherwise hand it an unusable root.
;; Note: this variable is read via `project--value-in-dir', so it must be
;; set globally (or in dir-locals) -- a buffer-local value has no effect.
(use-feature project
  :custom
  (project-vc-extra-root-markers '("Cargo.toml")))

;; eglot: Configure Rust-specific LSP settings.
;;
;; Note: unlike other language modules, flymake is not enabled explicitly
;; here -- eglot turns it on itself once connected.  Enabling it earlier
;; would run the `rust-ts-flymake' backend (standalone clippy-driver with
;; no knowledge of Cargo dependencies), whose in-flight check then races
;; with eglot's flymake takeover and signals "Can't find state" errors.
(use-feature eglot
  :hook (rust-ts-mode . my/rust-eglot-ensure)

  :preface
  (defun my/rust-eglot-ensure ()
    "Start eglot unless the buffer visits toolchain or registry sources.
Files under the rustup toolchains and the Cargo registry are library
sources: rust-analyzer cannot check them as projects (the standard
library's manifests even require nightly Cargo), so only manage them
through the server of the project they were reached from, if any."
    (let ((file (and buffer-file-name (expand-file-name buffer-file-name))))
      (unless (and file
                   (seq-some (lambda (dir) (string-prefix-p (expand-file-name dir) file))
                             (list (or (getenv "RUSTUP_HOME") "~/.rustup/")
                                   (or (getenv "CARGO_HOME") "~/.cargo/"))))
        (eglot-ensure))))

  (defun my/rust-analyzer-executable ()
    "Return the path to the rust-analyzer executable, or nil if not found.
Prefer rust-analyzer on PATH, falling back to the rustup-managed binary,
which some distributions (e.g. Arch) don't expose on PATH."
    (or (executable-find "rust-analyzer")
        (when-let* ((rustup (executable-find "rustup"))
                    (path (ignore-errors
                            (car (process-lines rustup "which" "rust-analyzer")))))
          (and (file-executable-p path) path))))

  :config
  ;; Configure rust-analyzer as the Rust language server, running clippy
  ;; instead of `cargo check' for richer diagnostics.
  (when-let* ((rust-analyzer (my/rust-analyzer-executable)))
    (add-to-list 'eglot-server-programs
                 `((rust-mode rust-ts-mode)
                   . (,rust-analyzer
                      :initializationOptions (:check (:command "clippy")))))))

;; cargo: Cargo command integration.  Provides `cargo-minor-mode' with
;; keybindings under C-c C-c for build, run, test, clippy, etc.
(use-package cargo
  :defer t

  :hook (rust-ts-mode . cargo-minor-mode))

(provide 'init-prog-rust)

;;; init-prog-rust.el ends here
