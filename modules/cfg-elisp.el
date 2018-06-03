;;; cfg-elisp.el --- setting up Emacs for Emacs Lisp programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:elisp-module-init ()
  "Entry function of elisp module for the cfg init system."
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'cfg:visit-ielm)

  (add-hook 'emacs-lisp-mode-hook #'cfg:elisp-hook)

  (cfg:install elisp-slime-nav
    (require 'elisp-slime-nav)
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-interaction-mode-hook
                    ielm-mode-hook
                    eshell-mode-hook))
      (add-hook hook #'turn-on-elisp-slime-nav-mode))))

;;;###autoload (cfg:auto-module "\\.el\\'" elisp)


(defun cfg:elisp-hook ()
  "A hook that is called when Emacs Lisp mode is loaded."
  (display-line-numbers-mode t)
  (yas-minor-mode t))


(defun cfg:visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (cfg:start-or-switch-to 'ielm "*ielm*"))

;;; cfg-elisp.el ends here
