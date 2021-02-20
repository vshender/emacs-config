;;; cfg-scala.el --- setting up Emacs for Scala programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:scala-module-init ()
  "Entry function of scala module for the cfg init system."
  (cfg:install scala-mode
    (cfg:with-local-autoloads
      (add-hook 'scala-mode-hook #'cfg:-scala-hook)))

  (cfg:install lsp-mode
    (cfg:with-local-autoloads
      ;; TODO
      (require 'lsp-modeline)
      (require 'lsp-completion)

      (add-hook 'scala-mode-hook #'lsp)
      (add-hook 'lsp-mode-hook #'lsp-completion-mode)
      (add-hook 'lsp-mode-hook #'lsp-lens-mode)
      ))

  (cfg:install lsp-metals
    (cfg:with-local-autoloads
      (setq lsp-metals-show-implicit-arguments t)
      (setq lsp-metals-treeview-show-when-views-received nil)))

  (cfg:install lsp-ui
    (require 'lsp-ui)
    (add-hook 'scala-mode-hook #'lsp-ui-mode)

    (setq lsp-ui-flycheck-enable t
          lsp-ui-sideline-enable t))

  (cfg:install posframe)

  (cfg:install dap-mode
    (cfg:with-local-autoloads
      (add-hook 'lsp-mode #'dap-mode)
      (add-hook 'lsp-mode #'dap-ui-mode)))

  (cfg:install sbt-mode
    (cfg:with-local-autoloads))
  )

;;;###autoload (cfg:auto-module "\\.scala\\'" scala)


(defun cfg:-scala-hook ()
  "A hook that is called when scala mode is loaded."

  ;; (lsp-completion-mode)
  ;; (lsp-lens-mode)

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-scala.el ends here
