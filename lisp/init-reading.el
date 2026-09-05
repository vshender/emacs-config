;;; init-reading.el --- Document reading configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for reading documents in Emacs including EPUB, PDF, and DjVu.
;; Uses nov.el for EPUB files, pdf-tools for PDF, and djvu.el for DjVu.

;;; Code:

;; nov: EPUB reader for Emacs with font customization and navigation.
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)

  :custom
  ;; Store reading positions in the var/ directory.
  (nov-save-place-file (expand-file-name "nov-places" my/var-dir)))

;; pdf-tools: Feature-rich PDF viewer with annotations, search, and links.
;; Replaces the built-in DocView for PDF files.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)

  :hook
  ;; Use dark colors for comfortable reading.
  (pdf-view-mode . pdf-view-midnight-minor-mode)

  :custom
  ;; Scale each page to the window width (this is also the default).
  (pdf-view-display-size 'fit-width)
  ;; Enable HiDPI support.
  (pdf-view-use-scaling t)

  :config
  (pdf-tools-install :no-query))

(use-package pdf-view-restore
  :after pdf-tools

  :hook
  (pdf-view-mode . pdf-view-restore-mode)

  :custom
  ;; Store reading positions in the var/ directory.
  (pdf-view-restore-filename (expand-file-name "pdf-view-restore" my/var-dir))
  ;; Key stored positions by full file name.  Keying them by base name (the
  ;; default) is safe only with the package's per-directory restore file;
  ;; in the single store above, every `main.pdf' would share one page number.
  (use-file-base-name-flag nil))

;; djvu: Major mode for viewing DjVu documents.  The package autoloads the
;; `.djvu' association onto `djvu-init-mode' itself, so no `:mode' is needed.
(use-package djvu
  :defer t

  :preface
  ;; Display the rendered page image instead of djvu's default text (OCR)
  ;; layer.
  (defun my/-djvu-enable-image-mode (read-buf)
    "Enable `djvu-image-mode' in READ-BUF once it is displayed.
Return READ-BUF, djvu's read buffer."
    (when (buffer-live-p read-buf)
      ;; `djvu-image-mode' scrolls and measures the selected window, which
      ;; still shows another buffer while `find-file' sets this one up, so
      ;; wait for READ-BUF to get a window of its own.
      (run-at-time
       0 nil
       (lambda ()
         (when-let* (((buffer-live-p read-buf))
                     (window (get-buffer-window read-buf t)))
           (with-selected-window window
             ;; `display-images-p' answers for the selected frame, so ask it
             ;; only once WINDOW's frame is the one selected.
             (when (and (display-images-p) (not djvu-image-mode))
               ;; Rendering runs `ddjvu' and signals when it fails; keep
               ;; that from aborting the visit and leave the text view up.
               (with-demoted-errors "Cannot display Djvu page: %S"
                 (djvu-image-mode 1))))))))
    read-buf)

  :config
  ;; `djvu-find-file' returns the read buffer with the document fully set up;
  ;; the `djvu-read-mode' hook would run before the page is bound.
  (advice-add 'djvu-find-file :filter-return #'my/-djvu-enable-image-mode))

(provide 'init-reading)

;;; init-reading.el ends here
