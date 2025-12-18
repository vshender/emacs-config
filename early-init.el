;;; early-init.el --- Early initialization  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file runs before the GUI is initialized and before package.el.
;; It increases GC threshold for faster startup, disables UI elements,
;; and disables package.el for Elpaca.

;;; Code:

;; Increase GC threshold during startup for faster loading.
;; After init, gcmh takes over GC management (see init-core.el).
(setq gc-cons-threshold most-positive-fixnum)

;; Disable UI elements early (before frame renders).
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Prevent flash of unstyled modeline (restored in init-ui.el).
(setq-default mode-line-format nil)

;; Disable package.el.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
