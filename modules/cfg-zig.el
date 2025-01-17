;;; cfg-zig.el --- setting up Emacs for Zig  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:zig-module-init ()
  "Entry function of zig module for the cfg init system."
  (cfg:install zig-mode
    (cfg:with-local-autoloads
      (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))))

  (add-hook 'zig-mode-hook #'cfg:-zig-hook))

;;;###autoload (cfg:auto-module "\\.zig\\'" zig)

;;; Enable zig for org files in order to add support to Babel.
;;;###autoload (cfg:auto-module "\\.org\\'" zig)


(defun cfg:-zig-hook ()
  "A hook that is called when zig mode is enabled."
  (display-line-numbers-mode t))

;;; cfg-zig.el ends here
