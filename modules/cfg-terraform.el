;;; cfg-terraform.el  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:terraform-module-init ()
  (cfg:install terraform-mode
    (require 'terraform-mode)
    (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))))

;;;###autoload (cfg:auto-module "\\.tf\\'" terraform)

;;; cfg-terraform.el ends here
