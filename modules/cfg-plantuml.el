;;; cfg-plantuml.el --- setting up PlantUML mode  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:plantuml-module-init ()
  "Entry function of plantuml module for the cfg init system."
  (cfg:install plantuml-mode
    (autoload 'plantuml-mode "plantuml-mode" "Major mode for PlantUML" t)
    (add-to-list 'auto-mode-alist '("\\.\\(plantuml\\|puml\\|pum\\|plu\\)\\'" . plantuml-mode))

    (setq plantuml-default-exec-mode 'jar)
    (setq plantuml-jar-path
          (pcase system-type
            ('darwin
             (car (file-expand-wildcards "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))
            ('gnu/linux
             "/usr/share/java/plantuml/plantuml.jar")))

    (add-hook 'plantuml-mode-hook #'cfg:-plantuml-hook)

    (setq plantuml-indent-level 0)

    (with-eval-after-load 'org
      (setq org-plantuml-jar-path plantuml-jar-path)

      (org-babel-do-load-languages
       'org-babel-load-languages
       (append org-babel-load-languages
               '((plantuml . t)))))))

;;;###autoload (cfg:auto-module "\\.puml\\'" plantuml)
;;;###autoload (cfg:auto-module "\\.plantuml\\'" plantuml)

;;; Enable plantuml for org files in order to add support to Babel.
;;;###autoload (cfg:auto-module "\\.org\\'" plantuml)


(defun cfg:-plantuml-hook ()
  "A hook that is called when plantuml mode is loaded."
  (display-line-numbers-mode t))

;;; cfg-plantuml.el ends here
