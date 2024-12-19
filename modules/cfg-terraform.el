;;; cfg-terraform.el  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:terraform-module-init ()
  "Entry function of terraform module for the cfg init system."
  (cfg:install terraform-mode
    (require 'terraform-mode)

    (add-hook 'terraform-mode-hook #'cfg:-terraform-hook)))

;;;###autoload (cfg:auto-module "\\.tf\\(vars\\)?\\'" terraform)


(defun cfg:-terraform-hook ()
  "A hook that is called when terraform mode is loaded."
  (display-line-numbers-mode t))

;;; cfg-terraform.el ends here
