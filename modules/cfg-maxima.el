;;; cfg-maxima.el --- setting up Emacs for Maxima CAS  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:maxima-module-init ()
  "Entry function of maxima module for the cfg init system."

  (let ((maxima-dir
         (cond
          ((eq system-type 'darwin) "/usr/local/share/emacs/site-lisp/maxima/")
          ((eq system-type 'gnu/linux) "/usr/share/emacs/site-lisp/maxima/"))))
    (when (and maxima-dir
               (file-directory-p maxima-dir))
      (add-to-list 'load-path maxima-dir)

      (autoload 'maxima-mode "maxima" "Maxima mode" t)
      (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
      (autoload 'maxima "maxima" "Maxima interaction" t)
      (autoload 'imath-mode "imath" "Imath mode for math formula input" t)

      (setq imaxima-use-maxima-mode-flag t)
      (setq imaxima-fnt-size "large")

      (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))

      (add-hook 'maxima-mode-hook #'cfg:-maxima-hook))))

;; Autoload maxima immediately.
;;;###autoload (cfg:maxima-module-init)


(defun cfg:-maxima-hook ()
  "A hook that is called when maxima mode is loaded."
  (display-line-numbers-mode t))

;;; cfg-maxima.el ends here
