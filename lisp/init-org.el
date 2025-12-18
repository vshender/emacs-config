;;; init-org.el --- Org-mode configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org-mode and related packages configuration.
;;
;; Includes:
;; - Core org-mode settings (agenda, TODO states, refile, clocking)
;; - org-modern for visual styling
;; - org-roam for Zettelkasten-style notes

;;; Code:

(defconst my/org-dir (file-truename "~/misc/org")
  "The directory for Org files.")

(defun my/-expand-org-filename (fname)
  "Get an absolute name for the Org file FNAME."
  (expand-file-name fname my/org-dir))

(defun my/-expand-org-filenames (fnames)
  "Get absolute names for the list of Org files FNAMES."
  (mapcar #'my/-expand-org-filename fnames))

(defconst my/org-agenda-files
  (my/-expand-org-filenames
   '("education.org" "personal.org" "projects.org" "work.org"))
  "The files to be used for agenda display.")

(defconst my/org-refile-targets
  `((,(my/-expand-org-filenames '("education.org" "personal.org" "projects.org")) :maxlevel . 1)
    (,(my/-expand-org-filenames '("work.org" "someday.org")) :maxlevel . 2))
  "Targets for refiling entries.")

(defconst my/org-roam-dir (my/-expand-org-filename "roam")
  "The directory for org-roam files.")

;; Org: Powerful organization and note-taking system.  Configuration includes
;; display settings, agenda, TODO states, and LaTeX preview.
(use-package org
  :custom
  ;; Directory with org files.
  (org-directory my/org-dir)
  ;; The files to be used for agenda display.
  (org-agenda-files my/org-agenda-files)
  ;; Default notes file.
  (org-default-notes-file (my/-expand-org-filename "inbox.org"))

  ;; Turn on `org-indent-mode' on startup.
  (org-startup-indented t)
  ;; Change the face of a headline if it is marked DONE.
  (org-fontify-done-headline t)
  ;; Try to get images width from #+ATTR_ORG.
  (org-image-actual-width nil)

  ;; List of TODO entry keyword sequences.
  (org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
     (sequence "SOMEDAY(S!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
     (sequence "TODELEGATE(>)" "DELEGATED(D@/!)" "|" "COMPLETE(x!/!)")))
  ;; Undone entries will block switching the dependent entries to DONE.
  (org-enforce-todo-dependencies t)
  ;; Unchecked boxes will block switching the parent to DONE.
  (org-enforce-todo-checkbox-dependencies t)

  ;; Use imagemagick for LaTeX preview.
  (org-preview-latex-default-process 'imagemagick)
  ;; LaTeX preview process configuration.
  (org-preview-latex-process-alist
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

  :config
  ;; LaTeX packages for Russian language support and TikZ.
  (add-to-list 'org-latex-packages-alist '("T2A" "fontenc" t))
  (add-to-list 'org-latex-packages-alist '("english, russian" "babel" t))
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))

  ;; Enable TikZ preview in AUCTeX.
  (eval-after-load 'preview
    '(add-to-list 'preview-default-preamble
                  "\\PreviewEnvironment{tikzpicture}" t))

  :bind
  (("C-c o a" . org-agenda)
   ("C-c o g" . org-clock-goto)
   ("C-c o l" . org-store-link)
   ("C-c o c" . org-capture)
   ("C-c C-w" . org-refile))

  :hook (org-mode . visual-line-mode))

;; org-clock: Time tracking for org-mode tasks.  Provides clocking in/out
;; functionality to measure time spent on tasks.
(use-feature org-clock
  :after org
  :custom
  ;; Resume clocking task on clock-in if the clock is open.
  (org-clock-in-resume t)
  ;; Change task state to STARTED when clocking in.
  (org-clock-in-switch-to-state "STARTED"))

;; org-capture: Quick capture of ideas, tasks, and notes into org files
;; without disrupting workflow.
(use-feature org-capture
  :after org
  :custom
  (org-capture-templates
   `(;; Quick task capture with timestamp and optional link to clocked task.
     ("i" "Inbox" entry (file ,(expand-file-name "inbox.org" my/org-dir))
      "* TODO %?\nEntered on %T\n%(if (org-clocking-p) \"Clocked: %K\" \"\")\n%i")
     ;; Daily journal entry filed by date with time header.
     ("j" "Journal" entry (file+olp+datetree ,(expand-file-name "diary.org" my/org-dir))
      "*** %(format-time-string \"%H:%M\")\n%?"))))

;; org-refile: Move or copy org entries to different locations in org files.
;; Enables organizing captured items into appropriate project files.
(use-feature org-refile
  :after org

  :custom
  ;; Targets for refiling entries.
  (org-refile-targets my/org-refile-targets)
  ;; Show full paths for refiling.
  (org-refile-use-outline-path 'file)
  ;; Complete in a single step.
  (org-outline-path-complete-in-steps nil))

;; org-src: Source code block editing in org-mode.  Allows editing code blocks
;; in their native major mode via a dedicated buffer.
(use-feature org-src
  :after org
  :custom
  ;; No extra indentation in source blocks.
  (org-edit-src-content-indentation 0))

;; org-modern: Modern styling for org-mode with better visual appearance
;; for headlines, blocks, tables, and other org elements.
(use-package org-modern
  :config
  (with-eval-after-load 'org
    (global-org-modern-mode)))

;; org-modern-indent: Provides visual indentation for org-mode headers
;; and content, working seamlessly with org-modern.
(use-package org-modern-indent
  :ensure (:host github :repo "jdtsmith/org-modern-indent")
  :hook (org-mode . org-modern-indent-mode))

;; org-roam: Zettelkasten-style note-taking with bidirectional links.
;; Implements Roam Research concepts in Emacs using an SQLite database.
(use-package org-roam
  :after org

  :custom
  ;; Directory for org-roam files.
  (org-roam-directory my/org-roam-dir)
  ;; Node display template with tags.
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag)))
  ;; Capture templates for new notes.
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :if-new
      (file+head
       "%<%Y%m%d%H%M%S>-${slug}.org"
       "#+title: ${title}\n#+startup: overview\n#+startup: inlineimages\n#+startup: latexpreview\n")
      :unnarrowed t)))

  :config
  ;; Enable automatic database synchronization.
  (org-roam-db-autosync-mode)

  :bind
  (("<f12>" . org-roam-node-find)
   ("C-c r f" . org-roam-node-find)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r c" . org-roam-capture)
   ("C-c r l" . org-roam-buffer-toggle)
   ("C-c r g" . org-roam-graph)
   ("C-c r a a" . org-roam-alias-add)
   ("C-c r a r" . org-roam-alias-remove)
   ("C-c r r a" . org-roam-ref-add)
   ("C-c r r r" . org-roam-ref-remove)
   ("C-c r t a" . org-roam-tag-add)
   ("C-c r t r" . org-roam-tag-remove)))

;; org-roam-db: Database configuration for org-roam.
(use-feature org-roam-db
  :after org-roam
  :custom
  ;; Store database in var/ directory.
  (org-roam-db-location (expand-file-name "org-roam.db" my/var-dir)))

(provide 'init-org)

;;; init-org.el ends here
