;;; cfg-terraform.el  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:terraform-module-init ()
  (cfg:install terraform-mode
    (require 'terraform-mode)))

;;;###autoload (cfg:auto-module "\\.tf\\(vars\\)?\\'" terraform)

;;; cfg-terraform.el ends here
