;;; cfg-scheme.el --- setting up Emacs for Scheme programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:scheme-module-init ()
  "Entry function of scheme module for the cfg init system."
  (add-hook 'scheme-mode-hook #'cfg:-scheme-hook)

  (cfg:install geiser
    ;; Geiser elisp sources are in the "elisp" subdirectory.
    (let ((default-directory (expand-file-name "elisp" default-directory)))
      (cfg:with-local-autoloads
        (with-eval-after-load 'geiser
          (setq geiser-guile-load-init-file-p t))))))

;;;###autoload (cfg:auto-module "\\.scm\\'" scheme)
;;;###autoload (cfg:auto-module "\\.rkt\\'" scheme)


(defun cfg:-scheme-hook ()
  "A hook that is called when scheme mode is loaded."
  (setq geiser-active-implementations '(racket guile))

  (setq-local company-backends
              '(geiser-company-backend company-files))

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-scheme.el ends here
