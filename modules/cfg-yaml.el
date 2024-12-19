;;; cfg-yaml.el  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:yaml-module-init ()
  "Entry function of yaml module for the cfg init system."
  (cfg:install yaml-mode
    (require 'yaml-mode)

    (add-hook 'yaml-mode-hook #'cfg:-yaml-hook)))

;;;###autoload (cfg:auto-module "\\.ya?ml$" yaml)

;;; Enable yaml for org files in order to add support to Babel.
;;;###autoload (cfg:auto-module "\\.org\\'" yaml)


(defun cfg:-yaml-hook ()
  "A hook that is called when terraform mode is loaded."
  (display-line-numbers-mode t))

;;; cfg-yaml.el ends here
