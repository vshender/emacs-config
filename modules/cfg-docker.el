;;; cfg-docker.el --- setting up Emacs for Dockerfile editing  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:docker-module-init ()
  "Entry function of docker module for the cfg init system."
  (cfg:install dockerfile-mode
    (require 'dockerfile-mode)

    (add-hook 'dockerfile-mode-hook #'cfg:-docker-hook)))

;;;###autoload (cfg:auto-module "^Dockerfile$" docker)


(defun cfg:-docker-hook ()
  "A hook that is called when docker mode is enabled."
  (display-line-numbers-mode t))

;;; cfg-markdown.el ends here
