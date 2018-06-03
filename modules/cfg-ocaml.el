;;; cfg-ocaml.el --- setting up Emacs for OCaml programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:ocaml-module-init ()
  "Entry function of ocaml module for the cfg init system."
  (cfg:install tuareg-mode
    (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
    (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
      (add-to-list 'completion-ignored-extensions ext))

    (add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . tuareg-mode))

    (with-eval-after-load 'tuareg
      (setq tuareg-prettify-symbols-full t))

    (add-hook 'tuareg-mode-hook #'cfg:-ocaml-hook))

  (let ((opam-share (ignore-errors
                      (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))

      (autoload 'merlin-mode "merlin" nil t nil)
      (autoload 'utop-minor-mode "utop" nil t nil)
      (autoload 'ocp-setup-indent "ocp-indent" nil t nil)

      (with-eval-after-load 'utop
        (setq utop-command "opam config exec utop -- -emacs")))))

;;;###autoload (cfg:auto-module "\\.ml[iylp]?\\'" ocaml)


(defun cfg:-ocaml-hook ()
  "A hook that is called when tuareg mode is loaded."
  (merlin-mode t)
  (utop-minor-mode t)
  (ocp-setup-indent)

  (setq-local company-backends
              '(merlin-company-backend company-files))

  (when (functionp 'prettify-symbols-mode)
    (prettify-symbols-mode))

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-ocaml.ml ends here
