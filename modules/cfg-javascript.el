;;; cfg-javascript.el --- setting up Emacs for JavaScript programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:javascript-module-init ()
  "Entry function of javascript module for the cfg init system."
  (cfg:install js2-mode
    (cfg:with-local-autoloads
      (add-to-list 'auto-mode-alist '("\\.jsm?\\'" . js2-mode))

      (with-eval-after-load 'js2-mode
        (setq
         js-indent-level 2
         js2-idle-timer-delay 0.5)

        (define-key js2-mode-map (kbd "RET") 'js2-line-break))

      (add-hook 'js2-mode-hook #'cfg:-javascript-hook)))

  (cfg:install rjsx-mode
    (cfg:with-local-autoloads
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
      (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))))

  (cfg:install tide
    ;; Load tide immediately because we're going to patch
    ;; `tide-annotate-completion' function.
    (require 'tide)

    (setq tide-sync-request-timeout 10
          tide-completion-ignore-case t
          tide-default-mode "JS")

    (add-hook 'js-mode-hook #'tide-setup))

  (cfg:install nodejs-repl
    (cfg:with-local-autoloads)))

;;;###autoload (cfg:auto-module "\\.js[mx]?$" javascript)


(defun cfg:-javascript-hook ()
  "A hook that is called when js2 mode is loaded."
  (setq mode-name "js2")

  (setq-local tide-filter-out-warning-completions t)

  (setq-local company-backends
              '(company-tide company-files))

  (cfg:setup-flycheck-for-javascript)

  (define-key js-mode-map (kbd "M-?") #'tide-references)
  (define-key js-mode-map (kbd "C-c d") #'tide-documentation-at-point)

  (define-key js-mode-map (kbd "C-x C-e") #'nodejs-repl-send-last-expression)
  (define-key js-mode-map (kbd "C-c C-r") #'nodejs-repl-send-region)
  (define-key js-mode-map (kbd "C-c C-l") #'nodejs-repl-load-file)
  (define-key js-mode-map (kbd "C-c C-z") #'nodejs-repl-switch-to-repl)

  (display-line-numbers-mode t)
  (yas-minor-mode t))


(defun cfg:setup-flycheck-for-javascript ()
  "Setup flycheck for JavaScript."
  (let* ((project-root (projectile-project-root))
         (eslint-executable (expand-file-name "node_modules/eslint/bin/eslint.js" project-root)))
    (when (file-exists-p eslint-executable)
      (setq-local flycheck-javascript-eslint-executable eslint-executable)

      ;; eslint utility is available, so disable js2-mode errors highlighting.
      (setq-local js2-mode-show-parse-errors nil)
      (setq-local js2-mode-show-strict-warnings nil))))

;;; cfg-javascript.el ends here
