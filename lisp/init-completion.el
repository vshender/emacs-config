;;; init-completion.el --- Completion framework configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Completion configuration for both minibuffer (`completing-read') and
;; in-buffer (`completion-at-point') completion.
;;
;; Minibuffer: vertico + orderless + marginalia + consult + embark
;; In-buffer: corfu + cape

;;; Code:

;; Vertico: Performant and minimalistic vertical completion UI based on the
;; default completion system.  Provides a clean interface for minibuffer
;; completion with cycling support.
(use-package vertico
  :demand t

  :custom
  ;; Enable cycling for `vertico-next'/`vertico-previous'.
  (vertico-cycle t)
  ;; Show more candidates.
  (vertico-count 25)
  ;; Disable scroll margin.
  (vertico-scroll-margin 0)

  :config
  (vertico-mode 1)

  :bind
  (:map vertico-map
   ;; Use PgUp and PgDown keys for scrolling.
   ("<prior>" . vertico-scroll-down)
   ("<next>" . vertico-scroll-up)))

;; vertico-multiform: Allows different display modes for different completion
;; categories (e.g., buffer mode for imenu, bookmarks).
(use-feature vertico-multiform
  :after vertico

  :custom
  ;; Configure display modes per completion category.
  ;; - `buffer' mode shows candidates in a separate buffer for more space.
  ;; - `mouse' mode enables mouse selection of candidates.
  (vertico-multiform-categories
   '((imenu buffer)
     (bookmark buffer)
     (consult-location buffer mouse)
     (consult-grep buffer)))

  :config
  (vertico-multiform-mode 1))

;; vertico-directory: Provides commands for Ido-like directory navigation.
;; See: https://github.com/minad/vertico/wiki#make-vertico-and-vertico-directory-behave-more-like-ivyido
(use-feature vertico-directory
  :after vertico

  :init
  (defun my/-vertico-insert ()
    "Smart directory separator insertion for file completion.
If the current candidate is a directory, insert it via `vertico-insert'.
Otherwise, insert a literal slash character."
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))

  :hook
  ;; Tidy shadowed file names.
  (rfn-eshadow-update-overlay . vertico-directory-tidy)

  :bind
  ;; More convenient directory navigation commands.
  (:map vertico-map
   ("/"     . my/-vertico-insert)
   ("RET"   . vertico-directory-enter)
   ("DEL"   . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)))

;; orderless: Flexible completion style that matches space-separated patterns
;; in any order.
(use-package orderless
  :custom
  ;; The `basic' completion style is a fallback to ensure that completion
  ;; commands which rely on dynamic completion tables, e.g.,
  ;; `completion-table-dynamic' or `completion-table-in-turn', work correctly.
  (completion-styles '(orderless basic))
  ;; Enable partial-completion for files.
  (completion-category-overrides '((file (styles partial-completion))))
  ;; Disable defaults, use our settings.
  (completion-category-defaults nil)
  ;; Emacs 31: partial-completion behaves like substring.
  (completion-pcm-leading-wildcard t))

;; Marginalia: Provides helpful annotations next to completion candidates in
;; the minibuffer.  The information on display depends on the type of content.
(use-package marginalia
  :after vertico

  :config
  (marginalia-mode 1))

;; nerd-icons-completion: Adds icons to completion candidates using nerd-icons.
(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode 1))

;; Consult: Enhanced commands with live preview functionality.  Provides better
;; versions of `switch-to-buffer', `yank-pop', `goto-line', `imenu', `ripgrep',
;; etc.
(use-package consult
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap occur] . consult-line)
   ("M-s g" . consult-ripgrep)))

;; Embark: Context-aware actions on completion candidates.  Select first, then
;; decide what to do with it (export, collect, act).
(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)))

;; embark-consult: Integration between Embark and Consult, enabling preview
;; in embark-collect buffers.
(use-package embark-consult
  :after (consult embark)

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Corfu: Popup-based `completion-at-point' UI for in-buffer completion.
;; Modern replacement for company-mode with better integration.
(use-package corfu
  :after orderless

  :hook
  (;; Enable Corfu only for certain modes (global mode doesn't play well with
   ;; eshell if you run a repl).
   (prog-mode . corfu-mode)
   (shell-mode . corfu-mode)
   (eshell-mode . corfu-mode))

  :custom
  ;; Enable cycling for `corfu-next'/`corfu-previous'.
  (corfu-cycle t)
  ;; Enable auto completion.
  (corfu-auto t)
  ;; Minimum prefix length for auto completion.
  (corfu-auto-prefix 2)
  ;; Disable current candidate preview.
  (corfu-preview-current nil)
  ;; Use scroll margin.
  (corfu-scroll-margin 5))

;; corfu-popupinfo: Shows documentation popups for completion candidates.
(use-feature corfu-popupinfo
  :after corfu

  :hook (corfu-mode . corfu-popupinfo-mode)

  :custom
  ;; Delay before showing the popup (initial . subsequent).
  (corfu-popupinfo-delay '(0.25 . 0.1))
  ;; Don't hide the popup when candidate changes.
  (corfu-popupinfo-hide nil))

;; corfu-terminal: Enables corfu popup display in terminal (non-GUI) Emacs.
(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

;; kind-icon: Adds VSCode-style icons to corfu completion candidates.
(use-package kind-icon
  :after corfu
  :if (display-graphic-p)

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Cape: Completion At Point Extensions.  Provides additional completion
;; backends for dabbrev, file paths, etc.
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(provide 'init-completion)

;;; init-completion.el ends here
