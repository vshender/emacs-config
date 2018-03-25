;;; cfg-web.el --- setting up web-mode  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:web-module-init ()
  (cfg:install web-mode
    (cfg:with-local-autoloads
      (add-to-list 'auto-mode-alist '("\\.\\(xml\\|x?html?\\)\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.djhtml?\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.dtl\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.gotmpl\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.html.j2\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.html.mustache\\'" . web-mode))

      (with-eval-after-load 'web-mode
        (setq
         web-mode-markup-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-css-indent-offset 2

         web-mode-script-padding 2
         web-mode-style-padding 2

         web-mode-enable-current-column-highlight t)

        (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
        (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
        (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
        (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil)))

      (add-hook 'web-mode-hook #'cfg:-web-hook)))

  (cfg:install emmet-mode
    (cfg:with-local-autoloads
      (add-hook 'sgml-mode-hook #'emmet-mode)
      (add-hook 'web-mode-hook #'emmet-mode)

      (eval-after-load 'emmet-mode
        (setq
         emmet-preview-default t
         emmet-indentation 2)))))

;;;###autoload (cfg:auto-module "\\.\\(xml\\|x?html?\\)\\'" web)
;;;###autoload (cfg:auto-module "\\.djhtml\\'" web)
;;;###autoload (cfg:auto-module "\\.dtl\\'" web)
;;;###autoload (cfg:auto-module "\\.gohtml\\'" web)
;;;###autoload (cfg:auto-module "\\.gotmpl\\'" web)
;;;###autoload (cfg:auto-module "\\.html.j2\\'" web)
;;;###autoload (cfg:auto-module "\\.mustache\\'" web)


(defun cfg:-web-hook ()
  "A hook that is called when web mode is loaded."
  (linum-mode t))

;;; cfg-web.el ends here
