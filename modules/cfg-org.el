;;; cfg-org.el --- setting up Org mode  -*- lexical-binding: t -*-

;;; Code:

(defconst cfg:org-dir "~/misc/org"
  "The directory for Org files.")

(defun cfg:expand-org-filename (fname)
  "Get an absolute name for the Org file name FNAME."
  (expand-file-name fname cfg:org-dir))

(defun cfg:expand-org-filenames (fnames)
  "Get absolute names for the list of Org file names FNAMES."
  (mapcar #'cfg:expand-org-filename fnames))

(defconst cfg:org-journal-dir (cfg:expand-org-filename "diary")
  "The directory for org-journal files.")

(defconst cfg:org-roam-dir (cfg:expand-org-filename "roam")
  "The directory for org-roam files.")

(defconst cfg:org-agenda-files
  (cfg:expand-org-filenames '("education.org" "personal.org" "projects.org" "work.org"))
  "The files to be used for agenda display.")

(defconst cfg:org-refile-targets
  `((,(cfg:expand-org-filenames '("education.org" "personal.org" "projects.org")) :maxlevel . 1)
    (,(cfg:expand-org-filenames '("work.org" "someday.org")) :maxlevel . 2))
  "Targets for refiling entries.")

;;;###autoload
(defun cfg:org-module-init ()
  (cfg:install org-mode
    ;; Org elisp sources are in the "lisp" subdirectory.
    (let ((default-directory (expand-file-name "lisp" default-directory)))
      (cfg:with-local-autoloads

        ;; Keybinding.
        (global-set-key (kbd "\C-cl") #'org-store-link)
        (global-set-key (kbd "\C-ca") #'org-agenda)
        (global-set-key (kbd "C-c c") #'org-capture)
        (global-set-key (kbd "C-c C-w") #'org-refile)
        (global-set-key (kbd "C-c o g") #'org-clock-goto)
        (global-set-key (kbd "C-c o c") #'calendar)

        (with-eval-after-load 'org
          ;;(setq org-log-done t)
          ;;(setq org-adapt-indentation nil)

          (setq
           ;; Directory with Org files.
           org-directory cfg:org-dir

           ;; The files to be used for agenda display
           org-agenda-files cfg:org-agenda-files

           org-refile-targets cfg:org-refile-targets
           org-default-notes-file "~/misc/org/inbox.org"
           org-refile-use-outline-path 'file
           org-outline-path-complete-in-steps nil

           ;; Undone entries will block switching the dependent entries to DONE.
           org-enforce-todo-dependencies t

           ;; Unchecked boxes will block switching the parent to DONE.
           org-enforce-todo-checkbox-dependencies t)

          (setq
           ;; Turn on `org-indent-mode' on startup.
           org-startup-indented t

           ;; Hide the first N-1 stars in a headline.
           org-hide-leading-stars t

           ;; Fontify emphasized text.
           ;; '(org-fontify-emphasized-text t)

           ;; Fontify headlines marked DONE.
           org-fontify-done-headline t

           org-image-actual-width nil

           ;; List of TODO entry keyword sequences.
           org-todo-keywords
           '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
             (sequence "SOMEDAY(S!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
             (sequence "TODELEGATE(>)" "DELEGATED(D@/!)" "|" "COMPLETE(x!/!)"))

           ;; https://stackoverflow.com/questions/44811679/orgmode-change-code-block-background-color
           ;; (require 'color)
           ;; (set-face-attribute 'org-block nil :background
           ;;                     (color-darken-name
           ;;                      (face-attribute 'default :background) 5))
           )

          (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                         (file+headline ,(expand-file-name "inbox.org" cfg:org-dir) "Tasks")
                                         "* TODO %i%?")
                                        ;; ("s" "Someday" entry
                                        ;;  (file ,(expand-file-name "someday.org" cfg:org-dir))
                                        ;;  "* %i%?\n%U")
                                        ))

          ;; Clock setup
          (setq
           ;; Resume clocking task on clock-in if the clock is open
           org-clock-in-resume t
           ;; Change task state to STARTED when clocking in
           org-clock-in-switch-to-state "STARTED")

          ;; render formulas
          (setq org-preview-latex-default-process 'imagemagick)
          (setq org-preview-latex-process-alist
                '((dvipng :programs ("latex" "dvipng")
                          :description "dvi > png"
                          :message "you need to install the programs: latex and dvipng."
                          :image-input-type "dvi"
                          :image-output-type "png"
                          :image-size-adjust (1.0 . 1.0)
                          :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                          :image-converter ("dvipng -D %D -T tight -o %O %f")
                          :transparent-image-converter ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
                  (dvisvgm :programs ("latex" "dvisvgm")
                           :description "dvi > svg"
                           :message "you need to install the programs: latex and dvisvgm."
                           :image-input-type "dvi"
                           :image-output-type "svg"
                           :image-size-adjust (1.7 . 1.5)
                           :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                           :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
                  (imagemagick :programs ("latex" "magick")
                               :description "pdf > png"
                               :message "you need to install the programs: latex and imagemagick."
                               :image-input-type "pdf"
                               :image-output-type "png"
                               :image-size-adjust (1.0 . 1.0)
                               :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
                               :image-converter ("magick convert -density %D -trim -antialias %f -quality 100 %O"))))

          (add-to-list 'org-latex-packages-alist '("T2A" "fontenc" t))
          (add-to-list 'org-latex-packages-alist '("english, russian" "babel" t))
          (add-to-list 'org-latex-packages-alist '("" "tikz" t))

          (eval-after-load "preview"
            '(add-to-list 'preview-default-preamble
                          "\\PreviewEnvironment{tikzpicture}" t))

          (setq org-edit-src-content-indentation 0)

          ) ;; end of cfg:with-local-autoloads
        )

      (add-hook 'org-mode-hook #'cfg:-org-hook)))

  (cfg:-install-org-roam))

(defun cfg:-install-org-roam ()
  "Install and configure org-roam."

  (cfg:install org-roam
    (cfg:with-local-autoloads
      (let ((org-roam-dir-truename (file-truename cfg:org-roam-dir)))
        (unless (file-directory-p org-roam-dir-truename)
          (make-directory org-roam-dir-truename))

        (setq
         org-roam-directory org-roam-dir-truename
         org-roam-v2-ack t
         org-roam-capture-templates
         '(("d" "default" plain #'org-roam-capture--get-point
            "%?"
            :file-name "%<%Y%m%d%H%M%S>-${slug}"
            :head "#+title: ${title}\n#+startup: overview\n#+startup: inlineimages\n#+startup: latexpreview\n"
            :unnarrowed t))

         ;; TODO
         org-roam-capture-templates
         '(("d" "default" plain "%?" :if-new
            (file+head
             "%<%Y%m%d%H%M%S>-${slug}.org"
             "#+title: ${title}\n#+startup: overview\n#+startup: inlineimages\n#+startup: latexpreview\n")
            :unnarrowed t))
         )
        )

      (require 'org-roam)

      ;; Keybindings.
      (global-set-key (kbd "<f12>") #'org-roam-node-find)
      (global-set-key (kbd "C-c r f") #'org-roam-node-find)
      (global-set-key (kbd "C-c r i") #'org-roam-node-insert)
      (global-set-key (kbd "C-c r c") #'org-roam-capture)
      (global-set-key (kbd "C-c r l") #'org-roam-buffer-toggle)
      (global-set-key (kbd "C-c r g") #'org-roam-graph)

      (global-set-key (kbd "C-c r a a") #'org-roam-alias-add)
      (global-set-key (kbd "C-c r a r") #'org-roam-alias-remove)
      (global-set-key (kbd "C-c r r a") #'org-roam-ref-add)
      (global-set-key (kbd "C-c r r r") #'org-roam-ref-remove)
      (global-set-key (kbd "C-c r t a") #'org-roam-tag-add)
      (global-set-key (kbd "C-c r t r") #'org-roam-tag-remove)

      ;; (add-hook 'org-mode-roam-hook #'cfg:-org-roam-hook))))

      ;; Keep Org-roam DB automatically synchronized.
      (org-roam-db-autosync-mode)
      )))

;; Autoload org-mode immediately to override standard org-loaddefs.
;;;###autoload (cfg:org-module-init)

;; this we don't need
;; ;;;###autoload (cfg:auto-module "\\.org$" org)
;; ;;;###autoload (cfg:auto-module "\\.org.txt$" org)


(defun cfg:-org-hook ()
  "A hook that is called when org mode is enabled."
  (visual-line-mode t))

;;; cfg-org ends here
