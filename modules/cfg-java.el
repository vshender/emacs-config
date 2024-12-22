;;; cfg-java.el --- setting up Emacs for Java programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:java-module-init ()
  "Entry function of java module for the cfg init system."

  (cfg:install dap-mode
    (cfg:with-local-autoloads))

  (cfg:install lsp-mode
    (cfg:with-local-autoloads
      (add-hook 'java-mode-hook #'lsp)))

  (cfg:install lsp-ui
    (cfg:with-local-autoloads))

  (cfg:install lsp-java
    (cfg:with-local-autoloads))

  (add-hook 'java-mode-hook #'cfg:-java-mode-hook))

;;;###autoload (cfg:auto-module "\\.java$" java)


(defun cfg:-java-mode-hook ()
  "A hook that is called when java mode is loaded."
  (setq c-basic-offset 2)

  (with-eval-after-load 'lsp-ui
    (define-key lsp-ui-mode-map
                [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
                [remap xref-find-references] #'lsp-ui-peek-find-references)

    (setq lsp-ui-flycheck-enable t
          lsp-ui-sideline-enable t))

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-java.el ends here
