;;; 01-cfg-el-get.el --- setting up el-get  -*- lexical-binding: t -*-

;;; Commentary:

;; El-get set up should be done during Emacs start-up as early as possible.

;;; Code:

(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

;; Forbid package.el initializing anything automatically since we use el-get.
(setq package-enable-at-startup nil)

;; Lazy download el-get during the first start-up.
(unless (require 'el-get nil 'noerror)
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)

  (require 'el-get)
  (require 'el-get-elpa)

  (el-get-elpa-build-local-recipes)

  ;; This will cause el-get to download the freshest version of itself.
  (el-get 'sync))


(setq
 ;; Enable git shallow clone to save time and bandwidth.
 el-get-git-shallow-clone t

 ;; Tell el-get to look for local customizations for every package into
 ;; `~/.emacs.d/init-<package>.el'
 el-get-user-package-directory user-emacs-directory

 ;; Make the el-get logging verbose.
 ;; WARNING: makes init slower.
 ;; el-get-verbose t

 ;; Turn them for core modules off.
 el-get-use-autoloads nil)

;; Sometimes, we need to experiment with our own recipe, or override the
;; default el-get recipe to get around bugs.
(add-to-list 'el-get-recipe-path
             (expand-file-name "el-get-recipes" user-emacs-directory))


;; Define the helper macro to be used instead of `el-get-bundle'.  The reason
;; behind it is to get rid of `el-get-bundle' caching of the init code in the
;; `el-get-bundle-init-directory'.
(defmacro cfg:install (package &rest hook)
  "Install the given PACKAGE via el-get running an optional after-install HOOK
forms."
  (declare (indent 1))
  `(let ((el-get-sources '((:name ,package
                            :after (progn ,@hook)
                            :prepare nil))))
     (el-get 'sync ',package)))

(defmacro cfg:with-local-autoloads (&rest body)
  "Execute BODY after building (if not present) and loading the autoloads in
the `default-directory' (it is normally set to point to the package
installation directory by `el-get' during the package init).  The reason behind
this is not using the standard el-get autoloads.  Sometimes, for large
packages, it's too awkward to configure autoloads manually in the hooks of
`cfg:install'."
  (declare (indent defun))
  `(let ((loaddefs (expand-file-name "cfg-local-loaddefs.el" default-directory)))
     (unless (file-exists-p loaddefs)
       (let ((generated-autoload-file loaddefs))
         (message "CFG: Generating local autoloads: %s" loaddefs)
         (update-directory-autoloads default-directory)))
     (load-file loaddefs)
     ,@body))

;;; 01-cfg-el-get.el ends here
