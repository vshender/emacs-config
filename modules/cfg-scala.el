;;; cfg-scala.el --- setting up Emacs for Scala programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:scala-module-init ()
  "Entry function of scala module for the cfg init system."
  (cfg:install scala-mode
    (cfg:with-local-autoloads
      (add-hook 'scala-mode-hook #'cfg:-scala-hook)))

  (cfg:install dap-mode
    (cfg:with-local-autoloads))

  (cfg:install lsp-mode
    (cfg:with-local-autoloads
      (add-hook 'scala-mode-hook #'lsp)))

  (cfg:install lsp-metals
    (cfg:with-local-autoloads
      (setq lsp-metals-show-implicit-arguments t)
      (setq lsp-metals-treeview-show-when-views-received nil)))

  (cfg:install lsp-ui
    (cfg:with-local-autoloads))
    ;; (add-hook 'scala-mode-hook #'lsp-ui-mode)

  ;; (cfg:install posframe)

  (cfg:install sbt-mode
    (cfg:with-local-autoloads)))

;;;###autoload (cfg:auto-module "\\.scala\\'" scala)


(defun cfg:-scala-hook ()
  "A hook that is called when scala mode is loaded."

  (with-eval-after-load 'lsp-ui
    (define-key lsp-ui-mode-map
                [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
                [remap xref-find-references] #'lsp-ui-peek-find-references)

    (setq lsp-ui-flycheck-enable t
          lsp-ui-sideline-enable t))

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-scala.el ends here
