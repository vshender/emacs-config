;;; init-productivity.el --- Productivity tools configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Productivity tools including life visualization and time management:
;; - life-calendar: Visual life calendar showing weeks lived and remaining
;; - org-pomodoro: Pomodoro technique timer integrated with org-mode clocking

;;; Code:

;; life-calendar: Displays a visual calendar of your life in weeks, inspired by
;; the "Your Life in Weeks" concept.  Helps visualize time passed and remaining.
(use-package life-calendar
  :defer t)

;; alert: Cross-platform notification library used by org-pomodoro.
;; Uses libnotify (notify-send) on Linux and osx-notifier (terminal-notifier)
;; on macOS for desktop notifications.
(use-package alert
  :custom
  (alert-default-style (pcase system-type
                         ('gnu/linux 'libnotify)
                         ('darwin 'osx-notifier)
                         (_ 'message))))

;; org-pomodoro: Pomodoro technique timer integrated with org-mode clocking.
(use-package org-pomodoro
  :after org

  :init
  (defun my/org-pomodoro-skip-break ()
    "Skip the current break or overtime and start a new pomodoro.
When in overtime or break state, kill the current timer and immediately
start a new pomodoro on the heading at point."
    (interactive)
    (when (org-pomodoro-active-p)
      (org-pomodoro-kill))
    (org-pomodoro))

  :custom
  ;; Standard Pomodoro durations.
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 20)
  ;; Long break after every 4 pomodoros.
  (org-pomodoro-long-break-frequency 4)
  ;; Manual breaks: enter overtime when pomodoro ends, user triggers the break.
  (org-pomodoro-manual-break t)
  ;; Keep time of killed pomodoros in the clock log.
  (org-pomodoro-keep-killed-pomodoro-time t)
  ;; Do not clock break time in org-clock reports.
  (org-pomodoro-clock-break nil)

  :bind
  ("C-c o p" . org-pomodoro)
  ("C-c o P" . my/org-pomodoro-skip-break))

(provide 'init-productivity)

;;; init-productivity.el ends here
