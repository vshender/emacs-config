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
  (setq inhibit-startup-screen t)

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
  (setq split-height-threshold 255)

  ;; Don't beep, just subtly flash the modeline on alarms.
  (setq ring-bell-function
    (lambda ()
      (let ((orig-fg (face-foreground 'mode-line)))
        (set-face-foreground 'mode-line "#F2804F")
        (run-with-idle-timer 0.1 nil
                 (lambda (fg) (set-face-foreground 'mode-line fg))
                 orig-fg)))))

(defun cfg:-setup-diminish ()
  "Setup minor modes diminishing."
  (cfg:install diminish
    (eval-after-load "abbrev" '(diminish 'abbrev-mode))
    (eval-after-load "company" '(diminish 'company-mode))
    (eval-after-load "helm" '(diminish 'helm-mode))
    (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
    (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))))

(defun cfg:-setup-color-theme ()
  "Setup Emacs color theme."
  (cfg:install solarized-emacs
    (add-to-list 'custom-theme-load-path default-directory)
    (load-theme 'solarized-dark t)

    (setq
     solarized-high-contrast-mode-line nil
     solarized-use-less-bold t)))

(defun cfg:-setup-font ()
  (when (eq system-type 'darwin)
    (set-frame-font "Menlo-11" nil t)))

;;; 50-cfg-gui.el ends here
