;;; cfg-java.el --- setting up Emacs for Java programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:java-module-init ()
  "Entry function of java module for the cfg install system."
  (add-hook 'java-mode-hook #'cfg:-java-mode-hook)

  (cfg:install meghanada
    (cfg:with-local-autoloads
      (with-eval-after-load 'meghanada
        (setq meghanada-server-install-dir
              (expand-file-name "meghanada" cfg:var-dir)))

      (add-hook 'java-mode-hook #'meghanada-mode))))

;;;###autoload (cfg:auto-module "\\.java$" java)


(defun cfg:-java-mode-hook ()
  "A hook that is called when java mode is loaded."
  (setq c-basic-offset 2)

  (setq-local company-backends
              '(company-meghanada company-files))

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-java.el ends here
