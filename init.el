;;; init.el --- Main Emacs configuration entry point  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Main configuration file that sets up load paths and loads modular
;; configuration files from lisp/ directory.  Each module handles a specific
;; aspect of the configuration (packages, UI, completion, etc.).

;;; Code:

;; Directory for persistent data: saved settings, bookmarks, custom configs.
(defconst my/etc-dir (expand-file-name "etc" user-emacs-directory))

;; Directory for volatile data: caches, history, auto-generated state.
(defconst my/var-dir (expand-file-name "var" user-emacs-directory))

;; Ensure data directories exist.
(dolist (dir (list my/var-dir my/etc-dir))
  (unless (file-directory-p dir)
    (make-directory dir 'recursive)))

;; Redirect native compilation cache to the var/ directory.
(when (native-comp-available-p)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache" my/var-dir)))

;; Add lisp/ directory to load path for modular config files.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load configuration modules in order.
;; Note: init-elpaca must be loaded first to set up package management.
(require 'init-elpaca)                  ; package management (Elpaca)
(require 'init-core)                    ; core Emacs settings
(require 'init-ui)                      ; UI and theme
(require 'init-windows)                 ; window management
(require 'init-shell)                   ; shell/terminal (eshell, eat)
(require 'init-completion)              ; completion (vertico, corfu, etc.)
(require 'init-org)                     ; org-mode and org-roam
(require 'init-vcs)                     ; version control (magit, diff-hl)
(require 'init-llm)                     ; LLM integration (gptel, claude-code)
(require 'init-markdown)                ; Markdown configuration
(require 'init-productivity)            ; productivity tools
(require 'init-prog)                    ; general programming settings
(require 'init-prog-elisp)              ; Emacs Lisp configuration
(require 'init-prog-json)               ; JSON configuration
(require 'init-prog-python)             ; Python configuration
(require 'init-prog-yaml)               ; YAML configuration

;;; init.el ends here
