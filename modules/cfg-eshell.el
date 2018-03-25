;;; cfg-eshell.el --- setting up Eshell  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:eshell-module-init ()
  "Entry function of eshell module for the cfg init system."
  (with-eval-after-load 'eshell
    (setq eshell-directory-name
          (expand-file-name "eshell" cfg:var-dir))))

;;;###autoload (cfg:eshell-module-init)

;;; cfg-eshell.el ends here
