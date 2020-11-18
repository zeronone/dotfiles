;;; config.el -*- lexical-binding: t; -*-

;; (setq comp-deferred-compilation t)

;;;
;;; Eager load
;;;

(add-to-list 'load-path "~/.doom.d/local")

;;
;; OS specific settings

;; yank hangs in Arch Linux Wayland
(when IS-LINUX
  (setq xclip-method 'wl-copy))


;;
;; Emacs startup
;;

;; load all packages when in deamon mode
(setq use-package-always-demand (daemonp))

;; improve startup time
(setq inhibit-compacting-font-caches t)

(setq x-super-keysym 'meta
      x-alt-keysym   'alt)

;; Initial frame size
(when window-system (set-frame-size (selected-frame) 150 50))

;; looks
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(menu-bar-mode -1)

;;
;; Editor Defaults
;;

;; Disable visual line mode
(visual-line-mode -1)
(setq truncate-lines nil)

;; default indent
(setq-default tab-width 2)

(setq-default user-full-name    "Arif Rezai"
              user-mail-address "me@arifrezai.com")

(setq
 ;; Line numbers are pretty slow all around. The performance boost of
 ;; disabling them outweighs the utility of always keeping them on.
 display-line-numbers-type nil)

;;
;; Definitions
;;
(defun arif/keymap--empty-command()
  "Keymap empty command."
  (interactive))


;;
;; Eagerly loaded packages
;;

;; lazy load for direnv was too late
(use-package! direnv
  :when (executable-find "direnv")
  :demand t
  :mode ("\\.envrc\\'" . +direnv-rc-mode)
  :config
  (direnv-mode +1))

(use-package! hydra)
(use-package! major-mode-hydra
  :demand t
  :config
  (map! "M-SPC" #'major-mode-hydra))
(use-package! pretty-hydra)


;;
;; which-key
;;
(after! which-key
  (setq which-key-show-early-on-C-h t)          ;; Bind C-h to show help
  (setq which-key-idle-delay 10000)             ;; don't show
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-setup-side-window-right-bottom))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modeline rightside cutoff
;; https://github.com/hlissner/doom-emacs/issues/2967
(setq all-the-icons-scale-factor 1.0)
(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker)))
(custom-set-faces!
  '(mode-line :family "Noto Sans" :height 1.0)
  '(mode-line-inactive :family "Noto Sans" :height 1.0))


;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(setq doom-font (font-spec :family "Hack" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
      doom-big-font (font-spec :family "Hack" :size 19))

;; Fira Mono doesn't have italics, so we highlight it instead.
(add-hook! doom-post-init
  (set-face-attribute 'italic nil :weight 'ultra-light :foreground "#ffffff"))

(add-hook! doom-big-font-mode
  (setq +doom-modeline-height (if doom-big-font-mode 37 29)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;;; :editor evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)
;;
;; Keybindings
(map!
 ;; Easier window navigation
 :n "C-h"   #'evil-window-left
 :n "C-j"   #'evil-window-down
 :n "C-k"   #'evil-window-up
 :n "C-l"   #'evil-window-right)

;;
;; Treemacs
;;

(setq +treemacs-git-mode 'simple)
(after! treemacs
  (setq treemacs-position 'right)
  (setq treemacs-width 60))

;; treemacs
(after! treemacs-evil
  (define-key! treemacs-mode-map
    "h" nil
    "l" nil)

  (evil-define-key 'treemacs treemacs-mode-map (kbd "h") nil)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "l") nil)

  (define-key! evil-treemacs-state-map
    "h" nil
    "l" nil)

  (map!
   (:after treemacs-evil
    (:map evil-treemacs-state-map
     "C-h" #'evil-window-left
     "C-l" #'evil-window-right))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lang/org
;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory (expand-file-name "~/Dropbox/orgs/"))
(setq +org-dir (expand-file-name "~/Dropbox/orgs/"))
(setq +org-export-directory (expand-file-name "~/Dropbox/orgs/.export"))
(setq org-attach-id-dir (expand-file-name "~/Dropbox/orgs/.attach"))
(setq org-download-image-dir (expand-file-name "~/Dropbox/orgs/.attach"))
(setq org-roam-directory "~/Dropbox/orgs/mywiki")

;; org-journal
(setq org-journal-file-type 'weekly)
(setq org-journal-carryover-items t)
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-dir "~/Dropbox/orgs/journal")
(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (format-time-string (pcase org-journal-file-type
                         (`daily "#+TITLE: Daily Journal (%Y%m%d)\n#+STARTUP: showall\n\n\n")
                         (`weekly "#+TITLE: Weekly Journal (%Y%m%d)\n#+STARTUP: showall\n\n\n")
                         (`monthly "#+TITLE: Monthly Journal (%Y%m)\n#+STARTUP: folded\n\n\n")
                         (`yearly "#+TITLE: Yearly Journal (%Y)\n#+STARTUP: folded\n\n\n"))
                       (org-journal--convert-time-to-file-type-time time))))
(setq org-journal-file-header 'org-journal-file-header-func)
(setq org-journal-enable-agenda-integration t)

(setq org-noter-notes-search-path "~/Dropbox/orgs/mywiki/notes")
(setq org-pdftools-search-string-separator "??")
(setq org-ellipsis " >")
(setq org-id-link-to-org-use-id t)

;; eldoc in org-mode src blocks recurses https://github.com/hlissner/doom-emacs/issues/2972
(after! org-eldoc
  (puthash "cpp" #'ignore org-eldoc-local-functions-cache)
  (puthash "python" #'ignore org-eldoc-local-functions-cache))

(after! org
  ;; scrolling in large org files is too slow if enabled
  (setq org-highlight-latex-and-related nil)

  ;; for performance
  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#why-is-scrolling-slow-in-emacsdoom
  (remove-hook 'org-mode-hook #'org-superstar-mode)
  (setq org-fontify-quote-and-verse-blocks nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-hide-leading-stars nil)
  ;; (setq org-startup-indented nil)

  (setq org-image-actual-width (/ (display-pixel-width) 3))

  (setq-default org-cycle-separator-lines 0)
  (setq-default org-agenda-inhibit-startup nil)

  ;; https://github.com/hlissner/doom-emacs/issues/3085
  (setq org-id-link-to-org-use-id 'use-existing)

  ;; html export
  (setq-default org-html-htmlize-output-type 'css)
  (setq-default org-html-head "<link rel=\"stylesheet\" href=\"http://dakrone.github.io/org.css\" type=\"text/css\" />")

  ;; use python3 in org-babel
  (setq org-babel-python-command "python3")

  (setq org-image-actual-width 400)
  (setq org-agenda-files
        (append
         (directory-files-recursively (expand-file-name "projects-gtd" +org-dir) "\\.org$")
         (list +org-dir)))

  (setq org-publish-project-alist
        '(("org-notes"
           :base-directory "~/Dropbox/orgs"
           :base-extension "org"
           :publishing-directory "~/myfiles/orgs_published/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t)
          ("org-static"
           :base-directory "~/Dropbox/orgs"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/myfiles/orgs_published/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org-attachments"
           :base-directory "~/Dropbox/.attach"
           :publishing-directory "~/myfiles/orgs_published/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org-all"
           :components ("org-notes" "org-static" "org-attachments"))))

  (setq org-refile-additional-targets-a
        (directory-files-recursively (expand-file-name "archive" +org-dir) "\\.org$"))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-refile-additional-targets-a :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (setq org-refile-use-cache t)
  (run-with-idle-timer 300 t (lambda ()
                               (org-refile-cache-clear)
                               (org-refile-get-targets)))

  ;; GTD
  (defun my/org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (org-narrow-to-subtree)
    (goto-char (point-max)))
  (setq +org-capture-projects-base-directory (expand-file-name "projects-gtd" org-directory))
  (defun my/org-capture-refile-pending-file ()
    (expand-file-name "refile-pending.org" org-directory))
  (defun my/org-capture-project-todo-file ()
    (expand-file-name "todo.org" (expand-file-name (doom-project-name) +org-capture-projects-base-directory)))
  (defun my/org-capture-project-notes-file ()
    (expand-file-name "notes.org" (expand-file-name (doom-project-name) +org-capture-projects-base-directory)))
  (defun my/org-capture-project-changelog-file ()
    (expand-file-name "changelog.org" (expand-file-name (doom-project-name) +org-capture-projects-base-directory)))
  (defun my/org-capture-study-file ()
    (expand-file-name "study.org" org-directory))
  (setq org-default-notes-file
        (expand-file-name +org-capture-notes-file org-directory)
        +org-capture-journal-file
        (expand-file-name +org-capture-journal-file org-directory)
        org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal note" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("s" "Study" entry
           (file+headline my/org-capture-study-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("r" "Add pending refile" entry
           (file+headline my/org-capture-refile-pending-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal entry" plain
           (function my/org-journal-find-location)
           "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
           :jump-to-captured t
           :immediate-finish t)

          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline my/org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline my/org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline my/org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t))))

(use-package! org-sidebar
  :after org)

(use-package! org-drill
  :after org
  :config

  ;; default is 2
  (setq org-drill-failure-quality 1)
  ;; okay to forget around 30%
  (setq org-drill-forgetting-index 30)
  ;; failure threshold to consider an item as a leech
  (setq org-drill-leech-failure-threshold 40)

  (set-popup-rules!
    '(("^\\*Org-Drill\\*$" :side bottom :size 3 :select t :quit nil :ttl 0)))

  ;; (unmap! :map org-drill-response-mode-map
  ;;   [return] "RET")
  (map! :map org-drill-response-mode-map
        [return] nil
        "C-c C-c" #'org-drill-response-rtn)

  (setq org-drill-presentation-prompt-with-typing t)
  (setq org-drill-left-cloze-delimiter "[hint][")
  (setq org-drill-maximum-items-per-session 60)
  (setq org-drill-maximum-duration 90)           ; 90 minutes
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t))

(use-package! org-web-tools
  :commands (org-web-tools-insert-link-for-url
             org-web-tools-insert-web-page-as-entry
             org-web-tools-read-url-as-org
             org-web-tools-convert-links-to-page-entries
             org-web-tools-archive-attach
             org-web-tools-archive-view)
  :after org
  :config

  (setq org-web-tools-archive-fn #'org-web-tools-archive--wget-tar)
  (setq org-web-tools-archive-hostname "archive.vn")
  (setq org-web-tools-archive-debug-level 'trace))

;; From: https://www.rousette.org.uk/archives/doom-emacs-tweaks-org-journal-and-org-super-agenda/
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                             :file-path "refile-pending\\.org")
                            (:name "Next to do"
                             :todo "NEXT"
                             :order 1)
                            (:name "Important"
                             :priority "A"
                             :order 6)
                            (:name "Today's tasks"
                             :file-path "journal/")
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Scheduled Soon"
                             :scheduled future
                             :order 8)
                            (:name "Overdue"
                             :deadline past
                             :order 7)
                            (:name "Meetings"
                             :and (:todo "MEET" :scheduled future)
                             :order 10)
                            (:discard (:not (:todo "TODO")))))))))))
  :config
  (org-super-agenda-mode))

;; elfeed
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

(after! deft
  (add-to-list 'deft-extensions "md")
  (setq deft-recursive t)
  (setq deft-directory "~/Dropbox/orgs")
  (setq deft-archive-directory "~/Dropbox/orgs")

  (map! :map deft-mode-map
        [escape] #'quit-window))

(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (advice-add 'pdf-view-mouse-set-region :override #'*pdf-view-mouse-set-region))

(after! org-noter
  (map! :map org-noter-doc-mode-map
        :vni "i" #'org-noter-insert-note
        "M-i" #'org-noter-insert-precise-note
        "M-p" #'org-noter-sync-prev-page-or-chapter
        "M-." #'org-noter-sync-current-page-or-chapter
        "M-p" #'org-noter-sync-next-page-or-chapter
        "C-M-p" #'org-noter-sync-prev-note
        "C-M-." #'org-noter-sync-current-note
        "C-M-n" #'org-noter-sync-next-note))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  (after! pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; term
;; multiterm scrolling
(setq multi-term-program "/bin/zsh")
(after! multiterm
  (setq multi-term-scroll-show-maximum-output nil)
  (setq multi-term-scroll-to-bottom-on-output t)
  (setq multi-term-dedicated-window-height 40))

;; multi-term
(after! term
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (setq term-buffer-maximum-size 10000))))

;; flycheck
(after! flycheck
  (setq flycheck-display-errors-delay 1.5)    ;; 1.5 seconds

  (setq flycheck-python-mypy-cache-dir
        (concat doom-local-dir ".mypy-cache/"))

  (add-hook 'flycheck-error-list-mode #'doom|mark-buffer-as-real))

;; magit
(setq +magit-hub-enable-by-default nil)
(setq +magit-hub-features nil)
(after! magit
  (when IS-MAC
    (setq magit-git-executable "/usr/local/bin/git"))

  ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

;;
;; Company
;;

;; tabnine
(use-package! company-tabnine
  :after company)
(use-package! company-try-hard
  :after company
  :config

  (global-set-key (kbd "C-SPC") #'company-try-hard)
  (define-key company-active-map (kbd "C-SPC") #'company-try-hard))

;; company
(after! company
  ;;  original: (not erc-mode message-mode help-mode gud-mode eshell-mode)
  (setq company-global-modes '(not org-mode erc-mode message-mode help-mode gud-mode eshell-mode))
  (setq company-idle-delay 0)

  ;; defaults
  (setq company-backends '(company-capf company-tabnine)))

(after! company-box
  (setq company-box-doc-enable t)
  (setq company-box-doc-delay 0.5))

;;
;; help
;;

;; enable word-wrap in help buffers
(add-hook! help-mode visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp customizations
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq lsp-auto-configure t)
(setq lsp-ui-doc-enable t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-modeline-diagnostics-enable nil)   ;; we already have flycheck in modeline
(setq lsp-modeline-diagnostics-scope :file)
(setq lsp-enable-dap-auto-configure t)
(setq lsp-lens-enable t)
(setq lsp-completion-provider :capf)
(setq lsp-enable-semantic-highlighting t)
(setq lsp-enable-links nil)
(setq lsp-headerline-breadcrumb-segments '(file symbols))
(setq lsp-diagnostics-provider :flycheck)

;; for performance
(setq lsp-ui-sideline-enable nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-log-io nil)
(setq lsp-idle-delay 0.5)
(setq lsp-ui-sideline-delay 1.0)
(setq lsp-print-performance nil)

(setq lsp-treemacs-theme "Idea")

(use-package! lsp-pyright
  :defer t)

(after! lsp-java
  (add-hook 'java-mode-hook #'lsp-java-lens-mode)

  ;; entry command is lsp-jt-browser
  (require 'lsp-jt)
  (add-hook 'lsp-jt-mode-hook #'lsp-jt-lens-mode)
  ;; somehow the keymap doesn't work, so reapply
  ;; map! doesn't work
  (define-key! lsp-jt-mode-map
    "x" #'lsp-jt-run
    "d" #'lsp-jt-debug
    "R" #'lsp-jt-browser-refresh)

  ;; Requiring lsp-java-boot doesn't work
  ;; (require 'lsp-java-boot)
  ;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

  (setq lsp-java-java-path "java"
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-references-code-lens-enabled t
        lsp-java-signature-help-enabled t
        lsp-java-implementations-code-lens-enabled t
        lsp-java-format-enabled t
        lsp-java-save-actions-organize-imports t
        lsp-java-autobuild-enabled t
        lsp-java-completion-enabled t
        lsp-java-completion-overwrite t
        lsp-java-completion-guess-method-arguments t
        lsp-java-format-comments-enabled t
        lsp-java-code-generation-use-blocks t
        lsp-java-code-generation-generate-comments t
        lsp-java-code-generation-to-string-limit-elements 0
        lsp-java-inhibit-message t)

  ;; taken from vwiss emacs config
  (major-mode-hydra-define java-mode nil
                           ("General"
                            (("ESC" arif/keymap--empty-command "Quit" :exit t)
                             ("l" lsp-java-lens-mode "Java Lens" :toggle t)
                             ("S" lsp-java-spring-initializr "Spring Initializr" :color teal)
                             ("o" lsp-java-organize-imports "Organize imports" :color teal)
                             ("B" lsp-java-build-project "Build projects" :color teal)
                             ("u" lsp-java-update-project-configuration "Update project configuration" :color teal)
                             ("n" lsp-java-actionable-notifications "Actionable notifications" :color teal)
                             ("y" lsp-java-update-user-settings "Update user settings" :color teal)
                             ("s" lsp-java-update-server "Update server instalation" :color teal)
                             ("d" lsp-java-dependency-list "View java dependencies" :color teal)
                             )
                            ""
                            (
                             ("z" lsp-java-generate-to-string "Generate toString" :color teal)
                             ("h" lsp-java-generate-equals-and-hash-code "Generate equals and hashCode" :color teal)
                             ("R" lsp-java-generate-overrides "Generate method overrides" :color teal)
                             ("g" lsp-java-generate-getters-and-setters "Generate getters and setters" :color teal)
                             ("c" lsp-java-extract-to-constant "Extract constant" :color teal)
                             ("a" lsp-java-add-unimplemented-methods "Add Unimplemented" :color teal)
                             ("p" lsp-java-create-parameter "Create parameter" :color teal)
                             ("f" lsp-java-create-field "Create field" :color teal)
                             ("k" lsp-java-create-local "Create local" :color teal)
                             ("m" lsp-java-extract-method "Extract" :color teal)
                             ("i" lsp-java-add-import "Add missing import" :color teal)
                             ))))

(after! lsp-mode

  ;; Workaround for issue #3274
  (setq-hook! '(lsp-managed-mode-hook)
    flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))

  ;; clangd
  (setq lsp-clients-clangd-args '("--compile-commands-dir=build"
                                  "--pch-storage=memory"
                                  "--background-index"
                                  "-j=4"))

  ;; Don't render documentation in minibuffer
  (setq lsp-signature-render-documentation nil)

  ;; details while completion
  (setq lsp-completion-show-detail nil)
  (setq lsp-completion-show-kind nil)

  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)

  ;; additional clients
  (require 'lsp-pyright)
  (require 'lsp-java)

  (set-popup-rules!
    '(("^\\*lsp-help\\*" :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :select t :quit t :ttl 0))))

;; Automatically call dap-hydra when execution stopped
(after! dap-mode
  (dap-auto-configure-mode 1)
  (add-hook 'dap-stopped-hook
          (lambda (_arg) (call-interactively #'dap-hydra))))

(after! lsp-ui
  ;; default lsp disables it
  (setq lsp-ui-doc-enable t)

  (setq
   ;; the max-width doesn't work for webkit
   ;; lsp-ui-doc-use-webkit t

   ;; only when position is top or bottom
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-alignment 'frame

   ;; avoid documentation being rendered as markdown
   ;; lsp-ui-doc-render-function 'nil

   lsp-ui-doc-delay 2.0
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-max-height 60
   lsp-ui-doc-max-width 90
   lsp-ui-doc-position 'top

   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t))

;; requires (lsp +peek) flag
(map! :after lsp-ui-peek
      :map lsp-ui-peek-mode-map
      "h" #'lsp-ui-peek--select-prev-file
      "j" #'lsp-ui-peek--select-next
      "k" #'lsp-ui-peek--select-prev
      "l" #'lsp-ui-peek--select-next-file)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; disable flycheck on escape key
(setq +flycheck-on-escape nil)
(after! flycheck

  (setq flycheck-check-syntax-automatically '(save))

  (set-popup-rules!
    '(("^\\*Flycheck errors\\*"
       :modeline nil :select nil :quit current
       :side bottom :slot 9999 :vslot 9999))))

;;;;;;;;; Custom functions
(defun revert-all-no-confirm ()
  "Revert all file buffers, without confirmation.
Buffers visiting files that no longer exist are ignored.
Files that are not readable (including do not exist) are ignored.
Other errors while reverting a buffer are reported only as messages."
  (interactive)
  (let (file)
    (dolist (buf  (buffer-list))
      (setq file  (buffer-file-name buf))
      (when (and file  (file-readable-p file))
        (with-current-buffer buf
          (with-demoted-errors "Error: %S" (revert-buffer t t)))))))

;; don't follow vc-symlinks
;; https://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
(setq vc-follow-symlinks nil)

;; rust
(setq rustic-lsp-server 'rust-analyzer)
(after! rustic
  (set-evil-initial-state! 'rustic-popup-mode 'emacs)
  (set-popup-rules!
    '(("^rustic-popup-buffer" :select t :quit t :side bottom :slot 1)
      ("^\\*rustic-compilation\\*" :select nil :quit t :side bottom :slot 2)))

  ;; 'rust-analyzer is not ready yet
  (setq rustic-lsp-server 'rust-analyzer))

(require 'minizinc-mode)
(add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))

(after! org-src
  ;; ~/.doom.d/local/ob-minizinc.el
  (require 'ob-minizinc))

;; themes
(use-package! modus-operandi-theme
  :init
  ;; disabled
  (setq modus-operandi-theme-rainbow-headings nil)
  (setq modus-operandi-theme-proportional-fonts nil)
  (setq modus-operandi-theme-scale-headings nil)
  (setq modus-operandi-theme-section-headings nil)
  ;; enabled
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-visible-fringes t
        modus-operandi-theme-3d-modeline t
        modus-operandi-theme-subtle-diffs t
        modus-operandi-theme-org-blocks t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2))
(use-package! modus-vivendi-theme
  :init
  ;; disabled
  (setq modus-vivendi-theme-rainbow-headings nil)
  (setq modus-vivendi-theme-proportional-fonts nil)
  (setq modus-vivendi-theme-scale-headings nil)
  (setq modus-vivendi-theme-section-headings nil)
  ;; enabled
  (setq modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-visible-fringes t
        modus-vivendi-theme-3d-modeline t
        modus-vivendi-theme-subtle-diffs t
        modus-vivendi-theme-org-blocks t
        modus-vivendi-theme-scale-1 1.05
        modus-vivendi-theme-scale-2 1.1
        modus-vivendi-theme-scale-3 1.15
        modus-vivendi-theme-scale-4 1.2))

;; doom-theme
(setq doom-theme 'modus-operandi)
;; (setq doom-theme 'modus-vivendi)

;; disable smartparens, scrolling large org files is very slow
;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; file-templates
(use-package! s
  :demand t)
(defvar +private-file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "The path to a directory of yasnippet folders to use for file templates.")
(after! yasnippets
  (add-to-list 'yas-snippet-dirs '+private-file-templates-dir 'append #'eq)
  (yas-reload-all))

;; oj
(setq oj-home-dir "~/Dropbox/oj")
(use-package! oj
  :init
  (setq oj-default-online-judge 'codeforces)
  :config
  (set-popup-rules!
    '(("^\\*oj - " :select nil :slot -1 :vslot -1 :size 0.3 :ttl 0))))

(use-package! package-lint)

;; ws-butler
;; Trim extra whitespace from modified lines
(setq ws-butler-keep-whitespace-before-point nil)

;; safe vars
(put 'lsp-java-vmargs 'safe-local-variable (lambda (_) t))
