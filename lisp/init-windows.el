;;; init-windows.el --- Window management  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Window management configuration including window layout undo/redo and
;; popup window handling.

;;; Code:

;; winner: Provides undo/redo for window configuration changes.
(use-feature winner
  :demand t

  :custom
  ;; Disable default keybindings; we define our own below.
  (winner-dont-bind-my-keys t)
  ;; Number of window configurations to remember.
  (winner-ring-size 25)

  :config
  (winner-mode 1)

  :bind
  (:map winner-mode-map
   ("C-c [" . winner-undo)
   ("C-c ]" . winner-redo)))

;; popper: Popup window management with grouping and quick toggle/cycle.
(use-package popper
  :demand t

  :custom
  ;; Group popups by project directory.
  (popper-group-function #'popper-group-by-directory)

  ;; Buffers to treat as popups.
  (popper-reference-buffers
   '("\\*Backtrace\\*"
     "\\*Async Shell Command\\*"
     "^\\*eshell.*\\*$"
     "^\\*shell.*\\*$"
     "^\\*term.*\\*$"
     "^\\*eat.*\\*$"
     compilation-mode))

  ;; Popup window height as a fraction of the frame.
  (popper-window-height 0.45)

  :config
  (popper-mode 1)
  (popper-echo-mode 1)

  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type)))

(provide 'init-windows)

;;; init-windows.el ends here
