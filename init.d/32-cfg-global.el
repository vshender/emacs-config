;; 32-cfg-global.el --- setting up global modes and core plugins  -*- lexical-binding: t -*-

;;; Code:

(defun cfg:global-module-init ()
  "Entry function of global module for the cfg init system."

  (cfg:-setup-ido)
  (cfg:-setup-recentf)
  (cfg:-setup-projectile))


(defun cfg:-setup-ido ()
  "Setup Interactive Do."
  (require 'ido)

  (custom-set-variables
   '(ido-everywhere t)

   '(ido-enable-last-directory-history t)
   '(ido-save-directory-list-file (expand-file-name "ido.last" cfg:var-dir))

   '(ido-enable-flex-matching t)
   '(ido-enable-prefix nil)
   '(ido-enable-case nil)

   '(ido-ignore-extensions t)

   '(ido-create-new-buffer 'always)
   '(ido-use-filename-at-point nil)
   '(ido-auto-merge-work-directories-length -1))

  (customize-set-variable 'ido-file-extensions-order '(".yml" ".yaml" ".retry"))
  (add-to-list 'completion-ignored-extensions ".retry")

  (ido-mode t)

  (cfg:install ido-ubiquitous
    (require 'ido-ubiquitous)
    (ido-ubiquitous-mode))

  (cfg:install smex
    (customize-set-variable
     'smex-save-file (expand-file-name "smex.hist" cfg:var-dir))

    (cfg:with-local-autoloads
      (global-set-key (kbd "M-x") #'smex)
      (global-set-key (kbd "M-X") #'smex-major-mode-commands))

    ;; This is the old M-x.
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(defun cfg:-setup-recentf ()
  "Setup recentf."
  (require 'recentf)

  (custom-set-variables
   `(recentf-save-file ,(expand-file-name "recentf" cfg:var-dir))
   '(recentf-max-saved-items 50)
   '(recentf-max-menu-items 25))

  (recentf-mode t)

  (defun cfg:ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))

  ;; get rid of `find-file-read-only' and replace it with something
  ;; more useful.
  (global-set-key (kbd "C-x C-r") 'cfg:ido-recentf-open))

(defun cfg:-setup-projectile ()
  "Setup projectile."
  (cfg:install projectile
    (require 'projectile)

    (custom-set-variables
     '(projectile-mode-line
       '(:eval (format " prj[%s]" (projectile-project-name))))
     '(projectile-known-projects-file
       (expand-file-name "projectile-bookmarks.eld" cfg:var-dir))
     '(projectile-use-git-grep nil))  ;; in order to grep in local config files

    (projectile-mode t)))

;;; 32-cfg-global.el ends here
