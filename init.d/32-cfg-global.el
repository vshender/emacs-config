;;; 32-cfg-global.el --- setting up global modes and core plugins  -*- lexical-binding: t -*-

;;; Code:

(defun cfg:global-module-init ()
  "Entry function of global module for the cfg init system."

  (cfg:-setup-term)
  (cfg:-setup-ido)
  (cfg:-setup-avy)
  (cfg:-setup-recentf)
  (cfg:-setup-ediff)
  (cfg:-setup-folding)
  (cfg:-setup-deft)
  (cfg:-setup-projectile)
  (cfg:-setup-helm)
  (cfg:-setup-autocompletion)
  (cfg:-setup-yasnippet)
  (cfg:-setup-flycheck)
  (cfg:-setup-magit)
  (cfg:-setup-ahg)
  (cfg:-setup-git-gutter))

;;{{{ Setup term
;; ----------------------------------------------------------------------------

(defun cfg:-setup-term ()
  ;; https://stackoverflow.com/questions/6820051/unicode-characters-in-emacs-term-mode
  (add-hook 'term-exec-hook
            (function
             (lambda ()
               (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

;;}}}

;;{{{ Setup ido
;; ----------------------------------------------------------------------------

(defun cfg:-setup-ido ()
  "Setup Interactive Do."
  (require 'ido)

  (setq
   ido-enable-last-directory-history t
   ido-save-directory-list-file (expand-file-name "ido.last" cfg:var-dir)

   ido-enable-flex-matching t
   ido-enable-prefix nil
   ido-enable-case nil

   ido-ignore-extensions t

   ido-create-new-buffer 'always
   ido-use-filename-at-point nil
   ido-auto-merge-work-directories-length -1)

  (setq ido-file-extensions-order '(".yml" ".yaml" ".retry"))
  (add-to-list 'completion-ignored-extensions ".retry")

  (ido-mode t)

  ;; (cfg:install ido-ubiquitous
  ;;   (require 'ido-ubiquitous)
  ;;   (ido-ubiquitous-mode))
  ;;
  ;; (cfg:install smex
  ;;   (cfg:with-local-autoloads
  ;;     (global-set-key (kbd "M-x") #'smex)
  ;;     (global-set-key (kbd "M-X") #'smex-major-mode-commands)
  ;;
  ;;     ;; This is the old M-x.
  ;;     (global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)
  ;;
  ;;     (with-eval-after-load 'smex
  ;;       (setq smex-save-file (expand-file-name "smex.hist" cfg:var-dir)))))
  )

;;}}}

;;{{{ Setup avy
;; ----------------------------------------------------------------------------

(defun cfg:-setup-avy ()
  "Setup avy."

  (cfg:install avy
    (cfg:with-local-autoloads
      (avy-setup-default)
      (global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1))))

;;}}}

;;{{{ Setup recentf
;; ----------------------------------------------------------------------------

(defun cfg:-setup-recentf ()
  "Setup recentf."
  (require 'recentf)

  (setq
   recentf-save-file (expand-file-name "recentf" cfg:var-dir)
   recentf-max-saved-items 50
   recentf-max-menu-items 25)

  (recentf-mode t)

  (defun cfg:ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))

  ;; Get rid of `find-file-read-only' and replace it with something
  ;; more useful.
  (global-set-key (kbd "C-x C-r") #'cfg:ido-recentf-open))

;;}}}

;;{{{ Setup ediff
;; ----------------------------------------------------------------------------

(defun cfg:-setup-ediff ()
  "Setup ediff."
  (setq
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-split-window-function #'split-window-horizontally)

  (defun cfg:command-line-diff (switch)
    (let ((file1 (pop command-line-args-left))
          (file2 (pop command-line-args-left)))
      (ediff file1 file2)))

  ;; Usage: emacs -diff file1 file2
  (add-to-list 'command-switch-alist '("diff" . cfg:command-line-diff)))

;;}}}

;;{{{ Setup folding
;; ----------------------------------------------------------------------------

(defun cfg:-setup-folding ()
  "Setup folding."
  (cfg:install folding
    (require 'folding)
    (folding-mode-add-find-file-hook)
    (setq
     ;; Enable Folding mode for all known major modes.
     folding-check-folded-file-function
     (lambda ()
       (assq major-mode folding-mode-marks-alist))

     ;; Don't fold buffers when starting Folding mode.
     folding-folding-on-startup nil))

  (cfg:install fold-dwim
    (require 'fold-dwim)
    (add-hook 'folding-mode-hook
              (lambda ()
                (require 'fold-dwim)

                (local-set-key (kbd "C-c f t") #'fold-dwim-toggle)
                (local-set-key (kbd "C-c f s") #'fold-dwim-show-all)
                (local-set-key (kbd "C-c f h") #'fold-dwim-hide-all)))))

;;}}}

;;{{{ Setup deft
;; ----------------------------------------------------------------------------

(defun cfg:-setup-deft ()
  "Setup deft."
  (cfg:install deft
    (cfg:with-local-autoloads
      (setq
       deft-directory (expand-file-name "misc/notes/notes" (getenv "HOME"))
       deft-extensions '("org" "txt" "md")
       deft-auto-save-interval 0))

    (global-set-key (kbd "<f11>") #'deft)))

;;}}}

;;{{{ Setup projectile
;; ----------------------------------------------------------------------------

(defun cfg:-setup-projectile ()
  "Setup projectile."
  (cfg:install projectile
    (require 'projectile)

    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

    (setq
     projectile-mode-line '(:eval (format " prj[%s]" (projectile-project-name)))
     projectile-known-projects-file (expand-file-name
                                     "projectile-bookmarks.eld" cfg:var-dir)
     projectile-cache-file (expand-file-name "projectile.cache" cfg:var-dir)
     projectile-use-git-grep nil  ;; in order to grep in local config files
     )

    (setq projectile-globally-ignored-file-suffixes
          '(".png" ".jpg" ".gif" ".svg" ".ico"))

    (projectile-mode t))

  ;; Setup ag in order to make `projectile-ag' work.
  (cfg:install ag
    (cfg:with-local-autoloads)))

;;}}}

;;{{{ Setup helm
;; ----------------------------------------------------------------------------

(defun cfg:-setup-helm ()
  "Setup helm."
  (cfg:install helm
    (cfg:with-local-autoloads
      ;; Use Ido to open files.
      (eval-after-load 'helm-mode
        '(add-to-list 'helm-completing-read-handlers-alist '(find-file . ido)))

      ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
      ;; Changed to "C-c h".
      (global-set-key (kbd "C-c h") #'helm-command-prefix)
      (global-unset-key (kbd "C-x c"))

      ;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
      (global-set-key (kbd "s-f")     #'helm-find-files)
      (global-set-key (kbd "C-x C-r") #'helm-recentf)
      (global-set-key (kbd "C-x b")   #'helm-mini)
      (global-set-key (kbd "s-b")     #'helm-mini)
      ;;(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
      (global-set-key (kbd "M-x")     #'helm-M-x)
      (global-set-key (kbd "M-y")     #'helm-show-kill-ring)
      (global-set-key (kbd "C-c h g") #'helm-do-grep-ag)
      (global-set-key (kbd "C-c h o") #'helm-occur)
      (global-set-key (kbd "C-c h x") #'helm-register)
      (global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)))

  ;; Set-up helm-swoop.
  (cfg:install helm-swoop
    (cfg:with-local-autoloads
      (global-set-key (kbd "M-s o") #'helm-swoop)
      (global-set-key (kbd "M-s /") #'helm-multi-swoop)

      ;; When doing isearch, hand the word over to helm-swoop.
      (define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch)))

  ;; Set-up helm-projectile.
  (cfg:install helm-projectile
    (cfg:with-local-autoloads))

  ;; Set-up helm-ag in order to make `helm-projectile-ag' work.
  (cfg:install helm-ag
    (cfg:with-local-autoloads))

  (with-eval-after-load 'helm
    (cfg:-helm-hook))
  (with-eval-after-load 'helm-swoop
    (cfg:-helm-swoop-hook)))

(defun cfg:-helm-hook ()
  "A hook that is called when helm is loaded."

  ;; Rebind TAB to run persistent action.
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  ;; Make TAB work in terminal.
  (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
  ;; List actions using C-z.
  (define-key helm-map (kbd "C-z") #'helm-select-action)

  (setq
   ;; Open helm buffer inside current window, not occupy whole other window.
   helm-split-window-inside-p t
   ;; Show current input in header-line of Helm buffer.
   helm-echo-input-in-header-line t
   ;; Move to end or beginning of source when reaching top or bottom of source.
   ;;'(helm-move-to-line-cycle-in-source t)

   ;; Search for library in `require' and `declare-function' sexp.
   helm-ff-search-library-in-sexp t
   ;; Scroll 8 lines when scrolling other window using M-<next>/M-<prior>.
   helm-scroll-amount 8

   ;; Use `recentf-list' instead of `file-name-history' during file opening.
   helm-ff-file-name-history-use-recentf t

   ;; Enable fuzzy matching.
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match    t
   helm-semantic-fuzzy-match   t
   helm-imenu-fuzzy-match      t
   helm-M-x-fuzzy-match        t
   helm-lisp-fuzzy-completion  t)

  (defun cfg:helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook #'cfg:helm-hide-minibuffer-maybe)

  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))

  ;; (setq
  ;;  ;; Limit the window height (works with autoresize-mode).
  ;;  helm-autoresize-min-height 15
  ;;  helm-autoresize-max-height 40)
  ;; (helm-autoresize-mode t)

  (helm-mode t)

  (helm-projectile-on)

  (setq
   projectile-completion-system #'helm
   projectile-switch-project-action #'helm-projectile))

(defun cfg:-helm-swoop-hook ()
  "A hook that is called when helm-swoop is loaded."
  ;; From helm-swoop to helm-multi-swoop-all.
  (define-key helm-swoop-map (kbd "M-i")
    'helm-multi-swoop-all-from-helm-swoop)

  ;; Move up and down like isearch.
  (define-key helm-swoop-map (kbd "C-r") #'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") #'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") #'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") #'helm-next-line))

;;}}}

;;{{{ Setup autocompletion
;; ----------------------------------------------------------------------------

(defun cfg:-setup-autocompletion ()
  "Setup autocompletion using company mode."

  (cfg:install company-mode
    (require 'company)

    (setq
     company-dabbrev-ignore-case t
     company-dabbrev-code-ignore-case t
     company-dabbrev-downcase nil
     company-minimum-prefix-length 2)

    (setq-default
     ;; Something universally applied.
     company-backends '((company-capf
                         :with company-dabbrev-code company-keywords)
                        company-files
                        company-dabbrev))

    ;; Use `company-complete-selection' instead of `company-complete-common' on
    ;; the Tab key press.
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)

    (global-company-mode))

  (cfg:install pos-tip
    (require 'pos-tip))

  (cfg:install company-quickhelp
    (require 'company-quickhelp)
    (company-quickhelp-mode 1)))

;;}}}

;;{{{ Setup yasnippet
;; ----------------------------------------------------------------------------

(defun cfg:-setup-yasnippet ()
  "Setup yasnippet."
  (cfg:install yasnippet
    (cfg:with-local-autoloads
      (with-eval-after-load 'yasnippet
        (yas-reload-all))))

  (cfg:install yasnippet-snippets
    (setq yas-snippet-dirs
          (list
           (expand-file-name "el-get/yasnippet-snippets/snippets"
                             user-emacs-directory)))))

;;}}}

;;{{{ Setup flycheck
;; ----------------------------------------------------------------------------

(defun cfg:-setup-flycheck ()
  "Setup flycheck."
  (cfg:install flycheck
    (require 'flycheck)
    (add-hook 'after-init-hook #'global-flycheck-mode)))

;;}}}

;;{{{ Setup magit
;; ----------------------------------------------------------------------------

(defun cfg:-setup-magit ()
  "Setup magit."
  ;; Obtain a complete magit repository instead of a shallow one in order to
  ;; make `magit-version' work.
  (let ((el-get-git-shallow-clone nil))
    (cfg:install magit
      ;; Magit elisp sources are in the "lisp" subdirectory.
      (let ((default-directory (expand-file-name "lisp" default-directory)))
        (cfg:with-local-autoloads
          (global-set-key (kbd "C-c g s") #'magit-status)
          (global-set-key (kbd "C-c g d") #'magit-dispatch-popup)

          (with-eval-after-load 'magit
            (setq magit-process-connection-type nil)))))))

;;}}}

;;{{{ Setup ahg
;; ----------------------------------------------------------------------------

(defun cfg:-setup-ahg ()
  "Setup ahg."
  (cfg:install ahg
    (cfg:with-local-autoloads)))

;;}}}

;;{{{ Setup git-gutter
;; ----------------------------------------------------------------------------

(defun cfg:-setup-git-gutter ()
  "Setup git-gutter."
  (cfg:install git-gutter
    (require 'git-gutter)
    (global-git-gutter-mode t)

    (global-set-key (kbd "C-c v =") 'git-gutter:popup-hunk)
    (global-set-key (kbd "C-c v p") 'git-gutter:previous-hunk)
    (global-set-key (kbd "C-c v n") 'git-gutter:next-hunk)
    (global-set-key (kbd "C-c v s") 'git-gutter:stage-hunk)
    (global-set-key (kbd "C-c v r") 'git-gutter:revert-hunk)))

;;}}}

;;; 32-cfg-global.el ends here
