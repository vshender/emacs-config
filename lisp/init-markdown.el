;;; init-markdown.el --- Markdown configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Markdown support with live preview and table of contents.

;;; Code:

;; markdown-mode: Major mode for Markdown files.
(use-package markdown-mode
  :preface
  (defun my/markdown-can-replace-dash ()
    "Return non-nil if a dash abbrev should expand at point.
Skips code blocks, inline code, and structural dash sequences at
the beginning of a line (HR, YAML divider, setext underline)."
    (let ((face (get-text-property (point) 'face)))
      (not (or
            ;; Inside a code block or inline code: dashes are literal.
            (memq face '(markdown-code-face
                         markdown-inline-code-face
                         markdown-pre-face
                         markdown-language-keyword-face))
            ;; Dashes start at column 0: likely a horizontal rule,
            ;; YAML front matter divider, or setext heading underline.
            (save-excursion
              (skip-chars-backward "-")
              (bolp))))))

  (defun my/markdown-expand-dash-after-trigger ()
    "Force dash abbrev expansion after typing a non-dash terminator.
`self-insert-command' may skip auto-expansion when the character
preceding the trigger has non-word syntax (which `-' often does),
so call `expand-abbrev' explicitly with point positioned just
after the dash run."
    (when (and abbrev-mode
               (not (eq last-command-event ?-))
               (not (eq (char-syntax last-command-event) ?w))
               (> (point) 1)
               (eq (char-before (1- (point))) ?-))
      (save-excursion
        (backward-char)
        (expand-abbrev))))

  (defun my/markdown-mode-setup ()
    "Per-buffer setup for `markdown-mode'."
    (add-hook 'post-self-insert-hook
              #'my/markdown-expand-dash-after-trigger
              nil t))

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :custom
  ;; Use pandoc for export and preview.
  (markdown-command "pandoc")
  ;; Use native syntax highlighting in fenced code blocks.
  (markdown-fontify-code-blocks-natively t)
  ;; Use variable-height fonts for headers.
  (markdown-header-scaling t)

  :config
  ;; Expand `--' to en-dash and `---' to em-dash in prose.
  (abbrev-table-put markdown-mode-abbrev-table
                    :regexp "\\(?:^\\|[^-]\\)\\(-+\\)")
  (define-abbrev markdown-mode-abbrev-table "---" "—" nil
    :enable-function #'my/markdown-can-replace-dash)
  (define-abbrev markdown-mode-abbrev-table "--" "–" nil
    :enable-function #'my/markdown-can-replace-dash)

  :bind
  (:map markdown-mode-map
   ("C-c C-e" . markdown-export)
   ("C-c C-v" . markdown-preview))

  :hook ((markdown-mode . abbrev-mode)
         (markdown-mode . my/markdown-mode-setup)))

;; markdown-toc: Generate table of contents for Markdown files.
(use-package markdown-toc
  :after markdown-mode

  :bind
  (:map markdown-mode-map
   ("C-c C-t" . markdown-toc-generate-or-refresh-toc)))

(provide 'init-markdown)

;;; init-markdown.el ends here
