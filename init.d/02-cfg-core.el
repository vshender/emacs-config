;;; 02-cfg-core.el --- core emacs setup to be done in the early init phase  -*- lexical-binding: t -*-

;;; Code:

(defun cfg:core-module-init ()
  "Entry function of core module for the cfg init system."

  (setq shell-file-name "/bin/zsh")

  ;; Ensure environment variables inside Emacs look the same as in the user's
  ;; shell.
  (cfg:install exec-path-from-shell
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize))

  ;; Set file for storing customization information.
  (setq custom-file (expand-file-name "custom.el" cfg:var-dir))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file)

  (cfg:-setup-cyrillic-shortcuts)
  (cfg:-setup-indenting)
  (cfg:-setup-whitespace)

  ;; Always select the help window.
  (setq help-window-select t)

  ;; Ask before closing Emacs.
  (when window-system
    (global-set-key (kbd "C-x C-c") 'cfg:ask-before-closing)))


(defun cfg:-setup-cyrillic-shortcuts ()
  "Fix problem with keyboard shortcuts in cyrillic keyboard layout."
  ;; Map Modifier-CyrillicLetter to the underlying Modifier-LatinLetter, so
  ;; that control sequences can be used when keyboard mapping is changed
  ;; outside of Emacs.
  ;;
  ;; For this to work correctly, .emacs must be encoded in the default coding
  ;; system.
  ;;
  ;; http://www.cofault.com/2011/12/cue-key.html
  (mapcar*
   (lambda (r e)  ; R and E are matching Russian and English keysyms
     ;; Iterate over modifiers.
     (mapc (lambda (mod)
             (define-key input-decode-map
               (vector (list mod r)) (vector (list mod e))))
           '(control meta super hyper))
     ;; Finally, if Russian key maps nowhere, remap it to the English key
     ;; without any modifiers.
     (define-key local-function-key-map (vector r) (vector e)))
   "йцукенгшщзхъфывапролджэячсмитьбю"
   "qwertyuiop[]asdfghjkl;'zxcvbnm,."))

(defun cfg:-setup-indenting ()
  "Setup text indenting."
  (setq-default
   tab-width 4
   standard-indent 4
   indent-tabs-mode nil
   fill-column 80))

(defun cfg:-setup-whitespace ()
  "Setup dealing with whitespace."
  ;; Always add new line to the end of a file.
  (setq-default require-final-newline t)

  ;; Fix trailing whitespace on file save.
  (add-hook 'before-save-hook #'whitespace-cleanup))


(defun cfg:ask-before-closing ()
  "Ask whether or not to close, and then close if 'y' was pressed"
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit Emacs? ")
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

;;; 02-cfg-core.el ends here
