;;; cfg-javascript.el --- setting up Emacs for JavaScript programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:javascript-module-init ()
  "Entry function of javascript module for the cfg init system."
  (cfg:install js2-mode
    (cfg:with-local-autoloads
      (add-to-list 'auto-mode-alist '("\\.js[mx]?$" . js2-mode))

      (with-eval-after-load 'js2-mode
        (custom-set-variables
         '(js-indent-level 2)
         '(js2-idle-timer-delay 0.5))

        (define-key js2-mode-map (kbd "RET") 'js2-line-break))))

  (cfg:install tern
    ;; Tern elisp sources are in the "emacs" subdirectory.
    (let ((default-directory (expand-file-name "emacs" default-directory)))
      (cfg:with-local-autoloads)))
  (cfg:install company-tern
    (cfg:with-local-autoloads))

  (cfg:install nodejs-repl
    (cfg:with-local-autoloads))

  (add-hook 'js2-mode-hook 'cfg:-javascript-hook))

;;;###autoload (cfg:auto-module "\\.jsx?$" javascript)


(defun cfg:-javascript-hook ()
  "A hook that is called when js2 mode is loaded."
  (setq mode-name "js2")

  (tern-mode t)

  (set (make-local-variable 'company-backends)
       '(company-tern company-files))

  (define-key js-mode-map (kbd "C-x C-e") #'nodejs-repl-send-last-sexp)
  (define-key js-mode-map (kbd "C-c C-r") #'nodejs-repl-send-region)
  (define-key js-mode-map (kbd "C-c C-l") #'nodejs-repl-load-file)
  (define-key js-mode-map (kbd "C-c C-z") #'nodejs-repl-switch-to-repl)

  (linum-mode t)
  (yas-minor-mode t))

;;; cfg-javascript.el ends here
