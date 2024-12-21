;;; cfg-scheme.el --- setting up Emacs for Scheme programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:scheme-module-init ()
  "Entry function of scheme module for the cfg init system."
  (cfg:install geiser
    ;; Geiser elisp sources are in the "elisp" subdirectory.
    (let ((default-directory (expand-file-name "elisp" default-directory)))
      (cfg:with-local-autoloads)))

  (cfg:install geiser-racket
    (cfg:with-local-autoloads))

  (add-hook 'scheme-mode-hook #'cfg:-scheme-hook))

;;;###autoload (cfg:auto-module "\\.scm\\'" scheme)
;;;###autoload (cfg:auto-module "\\.rkt\\'" scheme)


(defun cfg:-scheme-hook ()
  "A hook that is called when scheme mode is loaded."
  (display-line-numbers-mode t))

;;; cfg-scheme.el ends here
