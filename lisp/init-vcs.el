;;; init-vcs.el --- Version control configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Version control integration for Emacs:
;; - Magit: Comprehensive Git interface with staging, history, and dispatch
;; - diff-hl: Fringe indicators for uncommitted changes

;;; Code:

;; Magit: Feature-rich Git interface for Emacs.  Provides an intuitive
;; interface for all Git operations with excellent staging and history views.
(use-package magit
  :custom
  ;; Use pipes as a connection type for the Git process.
  (magit-process-connection-type nil)

  :bind
  (("C-c g s" . magit-status)
   ("C-c g d" . magit-dispatch)
   ("C-c g f" . magit-file-dispatch)))

;; diff-hl: Highlights uncommitted changes in the fringe (gutter).
;; Shows added, modified, and deleted lines with visual indicators.
(use-package diff-hl
  :hook
  ;; Update diffs when Magit refreshes.
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))

  :config
  (global-diff-hl-mode))

(provide 'init-vcs)

;;; init-vcs.el ends here
