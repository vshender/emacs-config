;;; cfg-cython.el --- setting up Emacs for Cython programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:cython-module-init ()
  "Entry function of cython module for the cfg init system."
  (cfg:install cython-mode
    (add-hook 'cython-mode-hook #'cfg:-cython-hook)))

;;;###autoload (cfg:auto-module "\\.pyx\\'" cython)
;;;###autoload (cfg:auto-module "\\.pxd\\'" cython)


(defun cfg:-cython-hook ()
  "A hook that is called when cython mode is enabled."
  (display-line-numbers-mode t))

;;; cfg-cython.el ends here
