;;; cfg-yaml.el  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:yaml-module-init ()
  (cfg:install yaml-mode
    (require 'yaml-mode)
    ;;(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
    ))

;;;###autoload (cfg:auto-module "\\.ya?ml$" yaml)

;;; cfg-yaml.el ends here
