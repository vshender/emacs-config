;;; init.el --- entry init script for Emacs configuration setup  -*- lexical-binding: t -*-

;;; Commentary:

;; The configuration core is based on ECFG Emacs configuration framework:
;; https://github.com/maslennikov/emacs-config.
;;
;; Configuration files are located in two directories:
;; - init.d: contains core Emacs configuration, universally applied to all
;;   Emacs activities;
;; - modules: contains configuration relevant for particular contexts (e.g.
;;   major mode set-ups).

;;; Code:

;;{{{ Config directories setup
;; ----------------------------------------------------------------------------

(defvar cfg:plugin-dir (expand-file-name "plugins" user-emacs-directory)
  "Directory for static plugins, i.e. not installed with el-get.")
(defvar cfg:var-dir (expand-file-name "var" user-emacs-directory)
  "Data directory, mainly for generated stuff, caches, state data, etc.")

(unless (file-directory-p cfg:plugin-dir)
  (make-directory cfg:plugin-dir 'recursive))
(unless (file-directory-p cfg:var-dir)
  (make-directory cfg:var-dir 'recursive))

(add-to-list 'load-path cfg:plugin-dir)
(let ((default-directory cfg:plugin-dir))
  (normal-top-level-add-subdirs-to-load-path))

;;}}}

;;{{{ Module loading helpers
;; ----------------------------------------------------------------------------

(defun cfg:module-init-hook (modulename)
  "Return the symbol for the init function of the module named MODULENAME."
  (intern (format "cfg:%s-module-init" modulename)))

(defun cfg:load-module (filename modulename)
  "Load an init file, where FILENAME is a full path to the loaded file.
After the file is loaded, the configuration system tries to run its init
function named `cfg:MODULENAME-module-init' if it's defined."
  (let ((init-hook (cfg:module-init-hook modulename)))
    (load-file filename)
    (if (fboundp init-hook)
        (funcall init-hook)
      (message "CFG: No module init-hook found: %s" init-hook))))


(defconst cfg:module-regexp ".*cfg-\\(.+\\)\\.el$"
  "Regexp which matches configuration module paths.
Configuration module is either init script or extension module.")

(defun cfg:run-cfg-modules (files)
  "Load every file listed in FILES thats name matches `cfg:module-regexp`."
  (dolist (filename files)
    (when (string-match cfg:module-regexp filename)
      (cfg:load-module (expand-file-name filename)
                       (match-string 1 filename)))))

;;}}}

;;{{{ Core initialization scripts loading
;; ----------------------------------------------------------------------------

(defconst cfg:init-script-regexp "^[0-9][0-9]-cfg-\\(.+\\)\\.el$"
  "Regexp which matches init script filenames.")

;; Load core initialization scripts from init.d directory.
(cfg:run-cfg-modules
 (directory-files (expand-file-name "init.d" user-emacs-directory)
                  t cfg:init-script-regexp))

;;}}}

;;{{{ Additional modules loading
;; ----------------------------------------------------------------------------

;; Extension modules are usually corresponding to the specific emacs
;; major-modes (e.g. js2-mode).  Their load is handled by autoloads.  Normally,
;; such a module exports its entry function as an autoload and then via the
;; autoload comment registers itself into the `auto-mode-alist' via
;; `cfg:auto-module'.

(defmacro cfg:auto-module (pattern module)
  "Autoload helper for config modules.
This function utilizes the `auto-mode-alist' to trigger the autoload of the
module."
  (let ((init-hook (cfg:module-init-hook module))
        (auto-hook-name (intern (format "cfg:-auto-module-hook-%s" module))))
    `(progn
       ;; Check missing init hook early: on loading the loaddefs.
       (unless (fboundp ',init-hook)
         (error "CFG: No auto-module init-hook found: %s" ',init-hook))

       ;; Define the auto-module init hook.
       (defun ,auto-hook-name ()
         ;; Remove itself from the `auto-mode-alist' upon the first call.
         (setq auto-mode-alist
               (rassq-delete-all ',auto-hook-name auto-mode-alist))
         ;; Run the module init-hook triggering the module autoload.
         (,init-hook)
         ;; Hoping that in hook the proper `auto-mode-alist' entry was
         ;; inserted.
         (set-auto-mode))

       ;; Register the auto-module hook to the `auto-mode-alist'.
       (add-to-list 'auto-mode-alist '(,pattern . ,auto-hook-name)))))

;;; Load all modules for the first time triggering el-get to install all stuff
;;; we need; for the subsequent runs the generated autoloads will be picked-up.
(require 'autoload)

(let ((cfg:module-dir (expand-file-name "modules" user-emacs-directory))
      (generated-autoload-file (expand-file-name "loaddefs.el" cfg:var-dir)))
  ;; Directory containing autoloads should be included because loaddefs has
  ;; relative paths.
  (add-to-list 'load-path cfg:var-dir)

  (if (file-exists-p generated-autoload-file)
      ;; Already been there, just load loaddefs.
      (load-file generated-autoload-file)
    ;; Making the first run, churn up all modules to trigger el-get installs.
    (cfg:run-cfg-modules (directory-files cfg:module-dir t))
    (update-directory-autoloads cfg:module-dir)))

;;}}}

;;; init.el ends here
