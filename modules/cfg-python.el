;;; cfg-python.el --- setting up Emacs for Python programming language  -*- lexical-binding: t -*-

;;; Code:

;;;###autoload
(defun cfg:python-module-init ()
  "Entry function of python module for the cfg init system."

  (setq cfg:virtualenvs-path "~/.virtualenvs")
  (setq cfg:default-virtualenv-path (expand-file-name "default" cfg:virtualenvs-path))

  (cfg:install company-jedi
    (cfg:with-local-autoloads)
    (autoload 'jedi:setup "jedi-core" nil t nil)
    (autoload 'jedi:install-server "jedi-core" nil t nil))

  (add-hook 'python-mode-hook #'cfg:-python-hook))

;;;###autoload (cfg:auto-module "\\.py\\'" python)


(defun cfg:-python-hook ()
  "A hook that is called when python mode is loaded."

  (cfg:setup-jedi)
  (cfg:setup-flycheck-for-python)

  (local-set-key (kbd "M-.") #'jedi:goto-definition)
  (local-set-key (kbd "M-,") #'jedi:goto-definition-pop-marker)

  (display-line-numbers-mode t)

  ;; In order to avoid `(void-function yasnippet-snippets--fixed-indent)`
  (require 'yasnippet-snippets)
  (yas-minor-mode t)

  (setq-local python-indent-def-block-scale 1))


;; (defun cfg:project-venv-name ()
;;   "Return a virtual environment name for the current python project."
;;   (let* ((project-name (projectile-project-name))
;;          (venv-path (expand-file-name project-name
;;                                       python-environment-directory)))
;;     (if (file-exists-p (expand-file-name "bin/activate" venv-path))
;;         project-name
;;       python-environment-default-root-name)))

(defun cfg:setup-jedi ()
  "Setup jedi."
  ;; In order to get the definition of `jedi:environment-virtualenv' as a
  ;; dynamic variable.
  (require 'jedi-core)

  (setq-local jedi:environment-root
              (format "~/.emacs.d/.python-environments/py%d"
                      (cfg:python-project-py-version)))

  ;; Install jediepcserver if it's not installed yet.
  (let ((jediepcserver-path (expand-file-name "bin/jediepcserver"
                                              jedi:environment-root)))
    (unless (file-exists-p jediepcserver-path)
      (message "CFG: install Jedi EPC server for %s venv"
               (file-name-nondirectory jedi:environment-root))

      (let ((jedi:environment-virtualenv
             (list "virtualenv"
                   (format "--python=python%d" (cfg:python-project-py-version))
                   "--quiet")))
        ;; `jedi:environment-virtualenv' is customized in order to specify
        ;; proper python version for the jedi virtual environment.
        (jedi:install-server))

      ;; Wait until jediepcserver installation is completed.
      (while (not (file-exists-p jediepcserver-path))
        (sleep-for 0 100))))

  (setq-local jedi:server-command
              (list (expand-file-name "bin/python" jedi:environment-root)
                    (expand-file-name "bin/jediepcserver" jedi:environment-root)))
  (setq-local jedi:server-args
              (list "--virtual-env" (cfg:python-project-venv-path)))

  (message "CFG: enable Jedi from %s venv"
           (file-name-nondirectory jedi:environment-root))
  (jedi:setup)

  (setq-local company-backends
              '(company-jedi company-files)))

(defun cfg:setup-flycheck-for-python ()
  "Setup flycheck for Python."
  (let ((venv-path (cfg:python-project-venv-path)))
    (setq-local flycheck-python-flake8-executable
                (expand-file-name "bin/flake8" venv-path))
    (setq-local flycheck-python-mypy-executable
                (expand-file-name "bin/mypy" venv-path))))

(defun cfg:python-project-venv-path ()
  "Return the path to the current Python project.

Use the default virtualenv if there is no virtualenv for the project."
  (let ((project-venv-path (expand-file-name ".venv" (projectile-project-root))))
    (if (file-exists-p (expand-file-name "bin/activate" project-venv-path))
        project-venv-path
      cfg:default-virtualenv-path)))

(defun cfg:python-project-py-version ()
  "Return a python version of the virtual environment for the current project."
  (if (file-exists-p (expand-file-name "bin/python3"
                                       (cfg:python-project-venv-path)))
      3
    2))

;;; cfg-python.el ends here
