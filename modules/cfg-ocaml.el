;; cfg-ocaml.el --- setting up Emacs for OCaml programming language  -*- lexical-binding: t -*-

;; opam install merlin utop ocp-indent

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

    ;; (with-eval-after-load 'tuareg
    ;;   (setq tuareg-prettify-symbols-full t))
    )

  (let ((opam-share (ignore-errors
                      (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))

      ;; (autoload 'merlin-mode "merlin" nil t nil)
      ;; (autoload 'merlin-company-backend "merlin-company" nil t nil)
      ;; (autoload 'utop-minor-mode "utop" nil t nil)
      ;; (autoload 'ocp-setup-indent "ocp-indent" nil t nil)
      (load "merlin")
      (load "merlin-company")
      (load "utop")
      (load "ocp-indent")

      (with-eval-after-load 'utop
        (setq utop-command "opam exec utop -- -emacs"))))

  (folding-add-to-marks-list 'tuareg-mode "(* {{{" "(* }}} *)" " *)" t)
  (add-hook 'tuareg-mode-hook #'cfg:-ocaml-hook))

;;;###autoload (cfg:auto-module "\\.ml[iylp]?\\'" ocaml)

;;; Enable ocaml for org files in order to add support to Babel.
;;;###autoload (cfg:auto-module "\\.org\\'" ocaml)


(defun cfg:-ocaml-hook ()
  "A hook that is called when tuareg mode is loaded."
  (merlin-mode t)
  (utop-minor-mode t)
  (ocp-setup-indent)

  (setq-local company-backends
              '(cfg:merlin-company-backend company-files))

  (when (functionp 'prettify-symbols-mode)
    (prettify-symbols-mode))

  (display-line-numbers-mode t)
  (yas-minor-mode t))


;;; Fix the keywords autocompletion issue.
(defvar cfg:ocaml-keywords
  '("and"
    "as"
    "assert"
    "begin"
    "class"
    "constraint"
    "do"
    "done"
    "downto"
    "else"
    "end"
    "exception"
    "external"
    "for"
    "fun"
    "function"
    "functor"
    "if"
    "in"
    "include"
    "inherit"
    "initializer"
    "lazy"
    "let"
    "match"
    "method"
    "module"
    "mutable"
    "new"
    "nonrec"
    "object"
    "of"
    "open"
    "open!"
    "private"
    "rec"
    "sig"
    "struct"
    "then"
    "to"
    "try"
    "type"
    "val"
    "virtual"
    "when"
    "while"
    "with")
  "List of OCaml keywords to be used for autocompletion.")

(defun cfg:find-keyword-completions (arg)
  "Find OCaml keyword completions that match the prefix ARG."
  (seq-filter
   (lambda (keyword) (string-prefix-p arg keyword))
   cfg:ocaml-keywords))

(defun cfg:merlin-company-backend (command &optional arg &rest _ignored)
  "Custom backend for the company-mode to enhance Merlin's autocompletion with
OCaml keywords."
  (interactive (list 'interactive))
  (cl-case command
    (candidates
     (let ((result (merlin-company-backend command arg))
           (keywords (cfg:find-keyword-completions arg)))
       (if keywords
           (append keywords result)
         result)))
    (annotation
     (if (member arg cfg:ocaml-keywords)
         " : keyword"
       (merlin-company-backend command arg)))
    (t
     (merlin-company-backend command arg))))

;;; cfg-ocaml.ml ends here
