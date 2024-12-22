;;; cfg-golang.el --- setting up Emacs for Go programming language  -*- lexical-binding: t -*-

;; go install golang.org/x/tools/gopls@latest

;;; Code:

;;;###autoload
(defun cfg:golang-module-init ()
  "Entry function of golang module for the cfg init system."
  (cfg:install go-mode
    (cfg:with-local-autoloads))

  (cfg:install lsp-mode
    (cfg:with-local-autoloads
      (add-hook 'go-mode-hook #'lsp-deferred)))

  (add-hook 'go-mode-hook #'cfg:-golang-hook))

;;;###autoload (cfg:auto-module "\\.go\\'" golang)


(defun cfg:-golang-hook ()
  "A hook that is called when go mode is loaded."
  (setq tab-width 2
        indent-tabs-mode 1)

  (exec-path-from-shell-copy-env "GOPATH")

  (local-set-key (kbd "C-c j") #'xref-find-definitions)  ;; also bound to `M-.'
  (local-set-key (kbd "C-c d") #'lsp-describe-thing-at-point)

  (local-set-key (kbd "C-c i a") #'go-import-add)    ; C-c C-a by default
  (local-set-key (kbd "C-c i r") #'go-remove-unused-imports)
  (local-set-key (kbd "C-c i g") #'go-goto-imports)

  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)

  (if (not (string-match "go" compile-command))
      (setq-local compile-command
                  "go build -v && go test -v && go vet"))

  (subword-mode t)
  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-golang.el ends here
