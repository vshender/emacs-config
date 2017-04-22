;;; cfg-ocaml.el --- setting up Emacs for OCaml programming language.  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:ocaml-module-init ()
  (cfg:install tuareg-mode
    (cfg:with-local-autoloads)

    (eval-after-load "tuareg"
      '(customize-set-variable 'tuareg-prettify-symbols-full t))

    (add-hook 'tuareg-mode-hook #'cfg:-ocaml-hook))

  (let ((opam-share (ignore-errors
                      (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))

      (autoload 'merlin-mode "merlin" nil t nil)
      (autoload 'utop-minor-mode "utop" nil t nil)
      (autoload 'ocp-setup-indent "ocp-indent" nil t nil)

      (custom-set-variables
       '(utop-command "opam config exec utop -- -emacs")))))

;;;###autoload (cfg:auto-module "\\.ml[iylp]?\\'" ocaml)


(defun cfg:-ocaml-hook ()
  (merlin-mode t)
  (utop-minor-mode t)
  (ocp-setup-indent)

  (set (make-local-variable 'company-backends)
       '(merlin-company-backend company-capf company-files))

  (when (functionp 'prettify-symbols-mode)
    (prettify-symbols-mode))

  (linum-mode t)
  (yas-minor-mode t))

;;; cfg-ocaml.ml ends here
