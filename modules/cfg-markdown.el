;;; cfg-markdown.el --- setting up Markdown mode  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:markdown-module-init ()
  (cfg:install markdown-mode
    (require 'markdown-mode))

  (add-hook 'markdown-mode-hook #'cfg:-markdown-hook))

;;;###autoload (cfg:auto-module "\\.md$" markdown)


(defun cfg:-markdown-hook ()
  "A hook that is called when markdown mode is loaded."
  (display-line-numbers-mode t))

;;; cfg-markdown.el ends here
