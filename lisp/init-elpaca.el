;;; init-elpaca.el --- setup Elpaca package manager  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file bootstraps Elpaca, a next-generation package manager for Emacs.
;;
;; This module must be loaded first before any other modules that use
;; `use-package' declarations.
;;
;; Key features configured here:
;;
;; - Elpaca bootstrap (self-installation on first run).
;; - `use-package' integration via `elpaca-use-package'.
;; - `use-feature' macro for built-in Emacs features.

;;; Code:

;; The following code is the official Elpaca installer snippet.
;; It handles first-time installation by cloning the Elpaca repository
;; and setting up the necessary directories.
;;
;; See https://github.com/progfolio/elpaca?tab=readme-ov-file#installer
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; On Windows, symlinks may not be available or may require elevated
;; privileges, so use file copying instead.
(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode))

;; `elpaca-use-package' bridges Elpaca with `use-package', allowing the
;; familiar `use-package' syntax to work with Elpaca's asynchronous package
;; installation.
(elpaca elpaca-use-package
  ;; Enable Elpaca support for `use-package's `:ensure' keyword.
  (elpaca-use-package-mode)
  ;; Make `:ensure t' the default for all `use-package' declarations,
  ;; so packages are automatically installed via Elpaca.
  (customize-set-variable 'use-package-always-ensure t))

;; `use-feature' is a convenience macro for configuring built-in Emacs
;; features.  Since `use-package-always-ensure' is set to `t' above, we need
;; a way to configure built-in features without triggering Elpaca installation.
(defmacro use-feature (name &rest args)
  "Configure a built-in Emacs feature using `use-package' syntax.

This macro is a wrapper around `use-package' with `:ensure nil',
which prevents Elpaca from trying to install the feature as an
external package.

Use this for built-in Emacs features (e.g., `dired', `org',
`recentf') instead of `use-package'.

NAME is the feature name (unquoted symbol).
ARGS are the same keyword arguments accepted by `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(provide 'init-elpaca)

;;; init-elpaca.el ends here
