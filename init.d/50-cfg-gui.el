;;; 50-cfg-gui.el --- configuring the look and feel  -*- lexical-binding: t -*-

;;; Code:

(defun cfg:gui-module-init ()
  "Entry function of gui module for the cfg init system."

  (cfg:-setup-basic-gui)
  (cfg:-setup-diminish)
  (cfg:-setup-color-theme)
  (cfg:-setup-font))


(defun cfg:-setup-basic-gui ()
  "Setup basic GUI for Emacs."

  ;; No splash screen on startup.
  (customize-set-variable 'inhibit-startup-screen t)

  ;; We don't need menubar (execpt OSX), toolbar nor scrollbar.
  (when (and (fboundp 'menu-bar-mode)
             (not (eq system-type 'darwin)))
    (menu-bar-mode -1))
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

  ;; The modeline configuration.
  (column-number-mode t)       ;; show column number
  (size-indication-mode t)     ;; show file size

  ;; Cursor configuration.
  (blink-cursor-mode -1)       ;; turn cursor blinking off

  ;; Text-related UI.
  (show-paren-mode t)          ;; turn visualization of matching parens on
  (global-hl-line-mode t)      ;; highlight current line

  ;; Increase splitting minimum height in order to prevent splitting a window
  ;; vertically.
  (customize-set-variable 'split-height-threshold 255))

(defun cfg:-setup-diminish ()
  "Setup minor modes diminishing."
  (cfg:install diminish
    (eval-after-load "abbrev" '(diminish 'abbrev-mode))
    (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))))

(defun cfg:-setup-color-theme ()
  "Setup Emacs color theme."
  (cfg:install solarized-emacs
    (load-theme 'solarized-dark t)

    (custom-set-variables
     '(solarized-high-contrast-mode-line t)
     '(solarized-use-less-bold t))))

(defun cfg:-setup-font ()
  (when (eq system-type 'darwin)
    (set-frame-font "Menlo-11" nil t)))

;;; 50-cfg-gui.el ends here
