;;; cfg-web.el  -*- lexical-binding: t -*-

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

      (eval-after-load 'web-mode
        '(progn
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
           (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))))))

  (cfg:install zencoding-mode
    (cfg:with-local-autoloads
      (add-hook 'sgml-mode-hook #'zencoding-mode)
      (add-hook 'web-mode-hook #'zencoding-mode)

      (eval-after-load 'zencoding-mode
        (setq zencoding-indentation 2))))

  (add-hook 'web-mode-hook #'cfg:-web-hook))

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


(define-advice zencoding-insert-and-flash
    (:around (orig-fun &rest args) move-point-inside-inserted-markup)
  "Move the point inside the just inserted by zencoding HTML markup."
  (save-excursion
    (apply orig-fun args))
  (re-search-forward ">\\(\n[[:blank:]]*\\)?</")
  (search-backward ">")
  (forward-char)
  (when (= (char-after) ?\n)
    (insert "\n")
    (indent-for-tab-command)))

;;; cfg-web.el ends here
