;;; init-ui.el --- UI configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; UI configuration including theme, fonts, window management, and popup handling.

;;; Code:

;; nord-theme: Arctic, north-bluish color palette with custom tweaks.
(use-package nord-theme
  :config
  ;; Load theme first
  (load-theme 'nord t)

  (let ((nord1 "#3B4252")   ; black
        (nord3 "#4C566A")   ; bright-black
        (nord5 "#E5E9F0")   ; white
        (nord6 "#ECEFF4")   ; bright-white
        (nord7 "#8FBCBB")   ; cyan
        (nord8 "#88C0D0")   ; bright-cyan
        (nord9 "#81A1C1")   ; blue
        (nord10 "#5E81AC")  ; bright-blue
        (nord11 "#BF616A")  ; red
        (nord12 "#D08770")  ; bright-red
        (nord13 "#EBCB8B")  ; yellow
        (nord14 "#A3BE8C")  ; green
        (nord15 "#B48EAD")  ; magenta
        ;; Index 20 is the brightest level in Nord theme's comment brightness
        ;; scale (0-20).
        (nord-comment (nth 20 nord-theme--brightened-comments)))
    ;; Make comments/docstrings brighter than the default color.
    (dolist (attr '(font-lock-comment-face
                    font-lock-comment-delimiter-face
                    font-lock-doc-face
                    custom-comment))
      (set-face-attribute attr nil :foreground nord-comment))

    ;; Apply brighter color to some eglot faces for better visibility.
    (with-eval-after-load 'eglot
      (dolist (attr '(eglot-parameter-hint-face
                      eglot-type-hint-face
                      eglot-diagnostic-tag-unnecessary-face))
        (set-face-attribute attr nil :foreground nord-comment)))

    ;; Brighten org blocks background.
    (with-eval-after-load 'org
      (set-face-attribute 'org-block nil :background "#333945"))

    ;; Customize ediff highlighting colors for better visibility with Nord theme.
    (with-eval-after-load 'ediff
      (set-face-attribute 'ediff-fine-diff-A nil :background "#772222")
      (set-face-attribute 'ediff-fine-diff-B nil :background "#227722"))

    ;; Customizes ANSI color faces to match the Nord theme palette.
    (with-eval-after-load 'ansi-color
      (set-face-attribute 'ansi-color-black nil :foreground nord1 :background nord1)
      (set-face-attribute 'ansi-color-red nil :foreground nord11 :background nord11)
      (set-face-attribute 'ansi-color-green nil :foreground nord14 :background nord14)
      (set-face-attribute 'ansi-color-yellow nil :foreground nord13 :background nord13)
      (set-face-attribute 'ansi-color-blue nil :foreground nord9 :background nord9)
      (set-face-attribute 'ansi-color-magenta nil :foreground nord15 :background nord15)
      (set-face-attribute 'ansi-color-cyan nil :foreground nord7 :background nord7)
      (set-face-attribute 'ansi-color-white nil :foreground nord5 :background nord5)
      (set-face-attribute 'ansi-color-bright-black nil :foreground nord3 :background nord3)
      (set-face-attribute 'ansi-color-bright-red nil :foreground nord12 :background nord12)
      (set-face-attribute 'ansi-color-bright-green nil :foreground nord14 :background nord14)
      (set-face-attribute 'ansi-color-bright-yellow nil :foreground nord13 :background nord13)
      (set-face-attribute 'ansi-color-bright-blue nil :foreground nord10 :background nord10)
      (set-face-attribute 'ansi-color-bright-magenta nil :foreground nord15 :background nord15)
      (set-face-attribute 'ansi-color-bright-cyan nil :foreground nord8 :background nord8)
      (set-face-attribute 'ansi-color-bright-white nil :foreground nord6 :background nord6))))

;; emacs: UI-related settings for visual appearance and behavior.
(use-feature emacs
  :custom
  ;; Don't show the splash screen.
  (inhibit-startup-screen t)

  ;; Increase splitting minimum height in order to prevent splitting a window
  ;; vertically.
  (split-height-threshold 120)
  ;; Increase splitting minimum width in order to prevent splitting a window
  ;; horizontally.
  (split-width-threshold 200)

  :config
  (when (eq system-type 'darwin)
    (defun my/-visible-bell-fn ()
      "Flash the first line and echo area like Linux visible-bell."
      (let* ((default-fg (face-foreground 'default))
             (flash-face `(:background ,default-fg :extend t))
             (overlays nil))
        ;; Create overlays for the first line in all top-row windows.
        (walk-windows
         (lambda (win)
           (when (zerop (nth 1 (window-edges win)))  ; window top edge at 0
             (let* ((buf (window-buffer win))
                    (start (window-start win))
                    (end (with-selected-window win
                           (save-excursion
                             (goto-char start)
                             (forward-line 1)
                             (point))))
                    (ov (make-overlay start end buf)))
               (overlay-put ov 'face flash-face)
               (overlay-put ov 'priority 9999)
               (push ov overlays))))
         nil 'visible)

        ;; Remove all overlays after delay.
        (run-with-timer
         0.1 nil
         (lambda ()
           (dolist (ov overlays)
             (delete-overlay ov))))

        ;; Flash the echo area by inverting the current message.
        ;; Use a 0-delay timer to run after the current command finishes.
        (run-with-timer
         0 nil
         (lambda ()
           (let ((msg (or (current-message) "")))
             (message "%s" (propertize
                            (concat msg (make-string (max 0 (- (frame-width) (length msg))) ?\s))
                            'face flash-face))
             (run-with-timer 0.1 nil #'message "%s" msg)))))))

  ;; Flash when the bell rings.
  (pcase system-type
    ('gnu/linux
     (setq visible-bell t))
    ('darwin
     ;; Mimic Linux visible-bell behavior: briefly invert the first line and
     ;; echo area.  The default `visible-bell' on macOS shows a huge
     ;; distracting exclamation mark icon.
     (setq ring-bell-function #'my/-visible-bell-fn)))

  (with-eval-after-load 'nord-theme
    ;; Restore modeline after theme is loaded (disabled in early-init.el to
    ;; prevent flash of unstyled modeline).
    (setq-default mode-line-format
                  (eval (car (get 'mode-line-format 'standard-value)))))

  ;; The modeline configuration.
  (column-number-mode 1)                ; show column number
  (size-indication-mode 1)              ; show file size

  ;; Turn cursor blinking off.
  (blink-cursor-mode -1)

  ;; Highlight current line in programming and text editing modes.
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)

  ;; Setup font.
  (set-frame-font
   (pcase system-type
     ('gnu/linux "Meslo LG S DZ-9")
     ('darwin "Menlo-12"))
   nil t))

(provide 'init-ui)

;;; init-ui.el ends here
