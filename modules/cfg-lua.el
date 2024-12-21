;;; cfg-lua.el --- setting up Emacs for Lua programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:lua-module-init ()
  "Entry function of lua module fo rthe cfg init system."
  (cfg:install lua-mode
    (cfg:with-local-autoloads
      (add-hook 'lua-mode-hook #'cfg:-lua-hook))))

;;;###autoload (cfg:auto-module "\\.lua\\'" lua)


(defun cfg:-lua-hook ()
  "A hook that is called when lua mode is loaded."
  (setq tab-width 4)
  (setq lua-indent-level 4)

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-lua.el ends here
