;;; 02-cfg-core.el --- core emacs setup to be done in the early init phase  -*- lexical-binding: t -*-

;;; Code:

(defun cfg:core-module-init ()
  """Entry function of core module for the cfg init system."""
  
  ;; Set file for storing customization information.
  (setq custom-file (expand-file-name "custom.el" cfg:user-dir))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file)

  ;; Always select the help window.
  (customize-set-variable 'help-window-select t))

(defun cfg:ask-before-closing ()
  "Ask whether or not to close, and then close if 'y' was pressed"
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit Emacs? ")
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Canceled exit")))
(when window-system
  (global-set-key (kbd "C-x C-c") 'cfg:ask-before-closing))

;; 02-cfg-core.el ends here
