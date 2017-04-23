;;; cfg-java.el --- setting up Emacs for Java programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:java-module-init ()
  "Entry function of java module for the cfg install system."
  (cfg:install meghanada
    (cfg:with-local-autoloads)

    (with-eval-after-load "meghanada"
      (customize-set-variable 'meghanada-server-install-dir
                              (expand-file-name "meghanada" cfg:var-dir))))

  (add-hook 'java-mode-hook 'cfg:-java-mode-hook))

;;;###autoload (cfg:auto-module "\\.java$" java)


(defun cfg:-java-mode-hook ()
  "A hook that is called when java mode is loaded."
  (setq c-basic-offset 2)

  (meghanada-mode t)
  (setq company-backends
        '((company-meghanada :with company-dabbrev-code)
          company-files))

  (linum-mode t)
  (yas-minor-mode t))

;;; cfg-java.el ends here
