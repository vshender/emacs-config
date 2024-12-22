;;; 31-cfg-functions.el --- some global function definitions  -*- lexical-binding: t -*-

;;; Code:

(defun cfg:start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))


(defun toggle-letter-accent-utf-8 ()
  "Toggle accent of the letter before the cursor."
  (interactive)
  (if (/= (char-before) #x301)
      (insert #x301)
    (delete-char -1)))

(global-set-key (kbd "C-c c a") #'toggle-letter-accent-utf-8)

;;; 31-cfg-functions.el ends here
