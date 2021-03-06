;;; cfg-golang.el --- setting up Emacs for Go programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:golang-module-init ()
  "Entry function of golang module for the cfg install system."
  (cfg:install go-mode
    (cfg:with-local-autoloads
      (add-hook 'go-mode-hook #'cfg:-golang-hook)))

  (cfg:install go-company))

;;;###autoload (cfg:auto-module "\\.go\\'" golang)


(defun cfg:-golang-hook ()
  "A hook that is called when go mode is loaded."
  (setq tab-width 2
        indent-tabs-mode 1
        fill-column 79)

  (when (executable-find "godef")
    (local-set-key (kbd "M-.")   #'godef-jump)
    (local-set-key (kbd "C-c j") #'godef-jump)       ; C-c C-j by default
    (local-set-key (kbd "C-c d") #'godef-describe))  ; C-c C-d by default

  (when (executable-find "godoc")
    (local-set-key (kbd "C-c C-d") #'godoc))

  (local-set-key (kbd "C-c i a") #'go-import-add)  ; C-c C-a by default
  (local-set-key (kbd "C-c i r") #'go-remove-unused-imports)
  (local-set-key (kbd "C-c i g") #'go-goto-imports)

  (setq-local company-backends
              '(company-go company-files))

  (add-hook 'before-save-hook #'gofmt-before-save nil t)

  (if (not (string-match "go" compile-command))
      (setq-local compile-command
                  "go build -v && go test -v && go vet"))

  (display-line-numbers-mode t)
  (yas-minor-mode t))

;;; cfg-golang.el ends here
