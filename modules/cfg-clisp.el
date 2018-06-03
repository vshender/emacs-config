;;; cfg-clisp.el --- setting up Emacs for Common Lisp programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:clisp-module-init ()
  "Entry function of clisp module for the cfg init system."
  (add-hook 'lisp-mode-hook #'cfg:-clisp-hook)

  (cfg:install sly
    (cfg:with-local-autoloads
      (setq inferior-lisp-program
            (case system-type
              (darwin "/usr/local/bin/sbcl")
              (gnu-linux "/usr/bin/sbcl")
              (otherwise "lisp")))
      (add-hook 'lisp-mode-hook #'sly-mode))))

;;;###autoload (cfg:auto-module "\\.li?sp\\'" clisp)
;;;###autoload (cfg:auto-module "\\.cl\\'" clisp)
;;;###autoload (cfg:auto-module "\\.sbclrc\\'" clisp)


(defun cfg:-clisp-hook ()
  "A hook that is called when Lisp mode is loaded."
  (display-line-numbers-mode t))

;;; cfg-clisp.el ends here
