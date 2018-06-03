;;; cfg-octave.el --- setting up Emacs for Octave programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:octave-module-init ()
  "Entry function of octave module for the cfg init system."
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

  (add-hook 'octave-mode-hook #'cfg:-octave-hook))

;;;###autoload (cfg:auto-module "\\.m\\'" octave)


(defun cfg:-octave-hook ()
  "A hook that is called when octave mode is loaded."
  (local-set-key (kbd "C-c C-z") #'run-octave)

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-octave.el ends here
