;;; init-core.el --- Core Emacs settings  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Core Emacs settings that should be loaded early in the initialization
;; process.

;;; Code:

;; gcmh: Garbage Collection Magic Hack --- optimizes GC by running it during
;; idle time rather than during active use, preventing UI stuttering.
(use-package gcmh
  :config
  (gcmh-mode 1))

;; emacs: Core Emacs settings including custom file, indentation, whitespace
;; handling, and Cyrillic keyboard support for using keybindings with Russian
;; layout.
(use-feature emacs
  :custom
  ;; Don't pop up UI dialogs when prompting.
  (use-dialog-box nil)

  ;; Width of a tab character for display purposes.
  (tab-width 4)
  ;; Use spaces instead of tabs for indentation.
  (indent-tabs-mode nil)

  ;; Column at which to wrap lines when using fill commands.
  (fill-column 79)

  ;; Always select the help window.
  (help-window-select t)

  ;; Whitespace handling: always add a new line to the end of a file.
  (require-final-newline t)

  ;; Directory prefix for auto-save list files (used for crash recovery).
  (auto-save-list-file-prefix
   (expand-file-name "auto-save-list/.saves-" my/var-dir))

  :config
  ;; Set file for storing customization information.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror 'nomessage)

  ;; On GNU/Linux, bind Home/End keys to beginning/end of buffer.
  (when (eq system-type 'gnu/linux)
    (global-set-key (kbd "<home>") #'beginning-of-buffer)
    (global-set-key (kbd "<end>") #'end-of-buffer))

  ;; Fix trailing whitespace on file save.
  (add-hook 'before-save-hook #'whitespace-cleanup)

  ;; Map Modifier-CyrillicLetter to the underlying Modifier-LatinLetter, so
  ;; that control sequences can be used when keyboard mapping is changed
  ;; outside of Emacs.
  ;;
  ;; This allows you to use Emacs keybindings without switching keyboard
  ;; layout from Russian to English.  For example, when Russian layout is
  ;; active, `C-ч' (the physical key where "x" is located) will be interpreted
  ;; as `C-x', making standard Emacs commands work regardless of the system
  ;; keyboard layout.
  ;;
  ;; For this to work correctly, .emacs must be encoded in the default coding
  ;; system.
  ;;
  ;; http://www.cofault.com/2011/12/cue-key.html
  (cl-mapcar
   (lambda (r e)  ; `r' and `e' are matching Russian and English keysyms
     ;; Iterate over modifiers.
     (mapc
      (lambda (mod)
        (define-key input-decode-map
                    (vector (list mod r)) (vector (list mod e))))
      '(control meta super hyper))
     ;; Finally, if Russian key maps nowhere, remap it to the English key
     ;; without any modifiers.
     (define-key local-function-key-map (vector r) (vector e)))
   "йцукенгшщзхъфывапролджэячсмитьбю"
   "qwertyuiop[]asdfghjkl;'zxcvbnm,."))

;; Start the Emacs server.
(use-feature server
  :config
  (unless (server-running-p)
    (server-start)))

;; tramp: Transparent Remote Access, Multiple Protocol.  Allows editing
;; remote files over SSH, SCP, and other protocols as if they were local.
(use-feature tramp
  :init
  ;; Store the TRAMP persistency file in the var/ directory to keep
  ;; ~/.emacs.d clean.
  ;;
  ;; `:init' with `setopt' is used instead of `:custom' here because
  ;; `tramp-persistency-file-name' is an autoloaded defcustom.  When using
  ;; `:custom', the value is set via `customize-set-variable', but when TRAMP
  ;; is lazily loaded later, its `defcustom' form re-evaluates and can restore
  ;; the default value.  Setting it directly in `:init' ensures the value is
  ;; already bound before TRAMP loads, and `defcustom' respects existing values.
  (setopt tramp-persistency-file-name (expand-file-name "tramp" my/var-dir)))

;; recentf: Tracks recently opened files for quick access.
(use-feature recentf
  :custom
  ;; Store the recentf file in the var/ directory.
  (recentf-save-file (expand-file-name "recentf" my/var-dir))
  ;; Maximum number of items to save.
  (recentf-max-saved-items 50)

  :config
  (recentf-mode 1))

;; savehist: Persists minibuffer history over Emacs restarts.
(use-feature savehist
  :custom
  ;; Store the history file in the var/ directory.
  (savehist-file (expand-file-name "history" my/var-dir))

  :config
  (savehist-mode 1))

;; saveplace: Remembers the last cursor position in each file and restores
;; it when the file is reopened.
(use-feature saveplace
  :custom
  ;; Store the places file in the var/ directory.
  (save-place-file (expand-file-name "places" my/var-dir))

  :config
  (save-place-mode 1))

;; bookmark: Save and jump to named locations in files.
(use-feature bookmark
  :custom
  ;; Store the bookmarks file in the var/ directory.
  (bookmark-file (expand-file-name "bookmarks" my/var-dir)))

;; which-key: Displays available keybindings in a popup after a delay.
(use-feature which-key
  :custom
  ;; Delay in seconds before the popup appears.
  (which-key-idle-delay 1.0)

  :config
  (which-key-mode 1))

;; url: Emacs's built-in URL retrieval library.  Used by many packages
;; (e.g., package.el, eww, elfeed) for fetching web content.
(use-feature url
  :custom
  ;; Store URL data (cookies, cache, history) in the var/ directory.
  (url-configuration-directory (expand-file-name "url" my/var-dir)))

;; exec-path-from-shell: Ensures environment variables inside Emacs match the
;; user's shell.  Essential for GUI Emacs to inherit PATH and other variables.
(use-package exec-path-from-shell
  :ensure (:wait t)

  :config
  ;; Initialize environment variables from the shell when running in a
  ;; graphical environment (macOS or X11) or as a daemon, since these don't
  ;; inherit the shell's environment.
  (when (or (memq window-system '(ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;; transient: Library for creating transient popup menus, used by Magit,
;; gptel, and other packages.
;;
;; gptel requires Transient 0.7.4 or higher.  The built-in Transient is of an
;; earlier version, so update it.
(use-package transient
  :custom
  ;; File to store values of transients.
  (transient-values-file (expand-file-name "transient/values.el" my/etc-dir))
  ;; File to store levels of transients and their suffixes.
  (transient-levels-file (expand-file-name "transient/levels.el" my/etc-dir))
  ;; File to store history of transients and their infixes.
  (transient-history-file (expand-file-name "transient/history.el" my/var-dir)))

(provide 'init-core)

;;; init-core.el ends here
