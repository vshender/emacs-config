;;; cfg-elisp.el --- setting up Emacs for Emacs Lisp programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:elisp-module-init ()
  "Entry function of elisp module for the cfg init system."
  (cfg:install elisp-slime-nav
    (cfg:with-local-autoloads
      (dolist (hook '(emacs-lisp-mode-hook
                      lisp-interaction-mode-hook
                      ielm-mode-hook
                      eshell-mode-hook))
        (add-hook hook 'turn-on-elisp-slime-nav-mode))))

  (defun cfg:visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (cfg:start-or-switch-to 'ielm "*ielm*"))
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'cfg:visit-ielm)

  (add-hook 'emacs-lisp-mode-hook #'cfg:elisp-hook))

;;;###autoload (cfg:auto-module "\\.el\\'" elisp)


(defun cfg:elisp-hook ()
  "A hook that is called when emacs lisp mode is loaded."
  (linum-mode t)
  (yas-minor-mode t))

;;; cfg-elisp.el ends here
