;;; cfg-scheme.el --- setting up Emacs for Scheme programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:scheme-module-init ()
  "Entry function of scheme module for the cfg init system."
  (cfg:install geiser
    ;; Geiser elisp sources are in the "elisp" subdirectory.
    (let ((default-directory (expand-file-name "elisp" default-directory)))
      (cfg:with-local-autoloads
        (with-eval-after-load "geiser"
          (custom-set-variables
           '(geiser-guile-load-init-file-p t))))))

  (defvar cfg:geiser-implementation
    (or (and (executable-find "racket") 'racket)
        (and (executable-find "guile") 'guile))
    "Symbol naming Scheme implementation to use.")

  (add-hook 'scheme-mode-hook #'cfg:-scheme-hook))

;;;###autoload (cfg:auto-module "\\.scm\\'" scheme)
;;;###autoload (cfg:auto-module "\\.rkt\\'" scheme)


(defun cfg:-scheme-hook ()
  "A hook that is called when scheme mode is loaded."
  (setq geiser-impl--implementation cfg:geiser-implementation)

  (set (make-local-variable 'company-backends)
       '(geiser-company-backend company-files))

  (linum-mode t)
  (yas-minor-mode t))

;;; cfg-scheme.el ends here
