;;; init-shell.el --- Shell and terminal configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Shell and terminal emulation configuration.

;;; Code:

;; eshell: Emacs shell written in Emacs Lisp, providing a shell interface
;; that works consistently across platforms.
(use-feature eshell
  :custom
  ;; Store eshell data (history, aliases, last-dir) in the var/ directory.
  (eshell-directory-name (expand-file-name "eshell" my/var-dir)))

;; eat: Emulate A Terminal --- terminal emulation inside Emacs.
(use-package eat
  :hook
  ;; Integrate with eshell for better terminal program support.
  (eshell-load . eat-eshell-mode))

(provide 'init-shell)

;;; init-shell.el ends here
