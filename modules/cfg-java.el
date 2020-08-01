;;; cfg-java.el --- setting up Emacs for Java programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:java-module-init ()
  "Entry function of java module for the cfg init system."

  (cfg:install lsp-ui
    (require 'lsp-ui)
    (add-hook 'java-mode-hook #'lsp-ui-mode)

    (setq lsp-ui-flycheck-enable t
          lsp-ui-sideline-enable t))

  (cfg:install lsp-java
    (require 'lsp-java)
    (require 'lsp-modeline)
    (add-hook 'java-mode-hook #'lsp))

  (cfg:install company-lsp
    (cfg:with-local-autoloads))

  (add-hook 'java-mode-hook #'cfg:-java-mode-hook))


;;;###autoload (cfg:auto-module "\\.java$" java)


(defun cfg:-java-mode-hook ()
  "A hook that is called when java mode is loaded."
  (setq c-basic-offset 2)

  (setq-local company-backends
              '(company-lsp company-files))

  (define-key lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map
    [remap xref-find-references] #'lsp-ui-peek-find-references)

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-java.el ends here
