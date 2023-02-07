;;; config.el -*- lexical-binding: t; -*-

;;;
;;; Load path
;;;
(add-to-list 'load-path "~/.doom.d/local")

;;
;; OS specific settings

;; yank hangs in Arch Linux Wayland
(when IS-LINUX
  (setq xclip-method 'wl-copy))

;; Same as defaults, just for reference
;; Redefining CMD to meta causes OSX specific keybindings to fail
;; For example: s-v is bound to yank
(when IS-MAC
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))

;;
;; Emacs startup
;;

;; load all packages when in deamon mode
(setq use-package-always-demand (daemonp))

;; Initial frame size
(when window-system (set-frame-size (selected-frame) 150 50))

;; Emacs 29 on mac supports precision scroll mode
(when EMACS29+
  (pixel-scroll-precision-mode +1))

;; looks
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(menu-bar-mode -1)

(setq window-divider-default-bottom-width 4)
(setq window-divider-default-right-width 4)


;; https://github.com/hlissner/doom-emacs/issues/5160
(setq doom-emoji-fallback-font-families nil)

;; Eagerly loaded libraries
(use-package! s
  :demand t)
(require 's)
(require 'dash)

;;
;; Editor Defaults
;;

;; Default 16 MB doesn't seem to be enough
;; (setq gcmh-verbose nil)
;; (setq-default gcmh-high-cons-threshold (* 64 1024 1024))

;; Disable auto-save
(setq auto-save-default nil)
(setq make-backup-files nil)

;; default indent
(setq-default tab-width 2)

(setq-default user-full-name    "Arif Rezai"
              user-mail-address "me@arifrezai.com")

;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

;;
;; Definitions
;;
(defun my/keymap--empty-command()
  "Keymap empty command."
  (interactive))

;; Found online
(defun my/garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

;;
;; Hydra (eagerly loaded)
;;
(use-package! hydra
  :demand t)
(use-package! major-mode-hydra
  :demand t
  :config
  (map! "C-c m" #'major-mode-hydra))
(use-package! pretty-hydra
  :demand t)

;; consult
(after! consult
  ;; Input debounce for commands like counsel-grep
  (setq consult-async-input-debounce 0.5))

;;
;; which-key
;;
(after! which-key
  (setq which-key-show-early-on-C-h t)          ;; Bind C-h to show help
  (setq which-key-idle-delay 0.5)             ;; don't show
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-setup-side-window-right-bottom))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modeline rightside cutoff
;; https://github.com/hlissner/doom-emacs/issues/2967
(setq all-the-icons-scale-factor 1.0)
;; (after! doom-modeline
;;   (doom-modeline-def-modeline 'main
;;     '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
;;     '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker)))
;; (custom-set-faces!
;;   '(mode-line :family "Noto Sans" :height 1.0)
;;   '(mode-line-inactive :family "Noto Sans" :height 1.0))


;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; Iosevka Fixed,Iosevka Fixed Medium:style=Medium,Regular
(when IS-MAC
  (setq doom-unicode-font (font-spec :family "Cascadia Code PL"))
  (setq doom-font (font-spec :family "Cascadia Code PL" :size 16)
        doom-variable-pitch-font (font-spec :family "Cascadia Code PL" :size 16)
        doom-big-font (font-spec :family "Cascadia Code PL" :size 22)))

(when IS-LINUX
  (setq doom-font (font-spec :family "Hack" :size 24)
        doom-variable-pitch-font (font-spec :family "Noto Sans" :size 24)
        doom-big-font (font-spec :family "Hack" :size 32)))

;; Fira Mono doesn't have italics, so we highlight it instead.
(add-hook! doom-post-init
  (set-face-attribute 'italic nil :weight 'ultra-light :foreground "#ffffff"))

(add-hook! doom-big-font-mode
  (setq +doom-modeline-height (if doom-big-font-mode 37 29)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;;; :editor evil
;; Vim (and evil) move the cursor one character back when exiting insert mode.
;; If you prefer that it didn’t, set:
(setq evil-move-cursor-back nil)
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; HACK
(defun +arif/insert-mode-escape (&optional interactive)
  "By default `+evil-escape-a' is called only after `#'evil-force-normal-state'
Which is bound to [escape] in normal mode.  We would like to also run
`doom-escape-hook' functions when [escape] is pressed in insert mode, and
potentially abort calling `#'evil-normal-state', if any returns non-nil value"
  (interactive (list 'interactive))
  (cond ;; Run all escape hooks. If any returns non-nil, then stop there.
   ((run-hook-with-args-until-success 'doom-escape-hook) t)
   ;; don't abort macros
   ((or defining-kbd-macro executing-kbd-macro) nil)))
(defun +arif/evil-insert-mode-escape-a (orig-fn &rest r)
  "Call `+arif/insert-mode-escape' if `evil-normal-state' is called interactively."
  (if (called-interactively-p 'any)
    (unless (call-interactively #'+arif/insert-mode-escape)
      (apply orig-fn r))
    (apply orig-fn r)))
(advice-add #'evil-normal-state :around #'+arif/evil-insert-mode-escape-a)

;; FIXME: doom/escape doesn't call keyboard-quit eventually
;; (after! evil
;;   (general-def 'insert 'override
;;     "ESC" #'doom/escape)
;;   (general-def 'insert 'override
;;     [escape] #'doom/escape))

;; Avy
(after! avy
  (setq avy-timeout-seconds 0.2)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-embark))


;; evil motion
(setq evil-snipe-scope 'buffer)
(use-package! evil-motion-trainer
  :after evil
  :config

  (emt-add-suggestion 'evil-next-line 'evil-avy-goto-char-timer)
  (emt-add-suggestion 'evil-previous-line 'evil-avy-goto-char-timer)

  ;;(global-evil-motion-trainer-mode +1)
)


;; Use ace-window rather than C-[hjkl]
;; window-select (ace-window)
(map! :map 'override "M-o" #'ace-window)

;;
;; Treemacs
;;
(setq treemacs-width 35)
(setq treemacs-user-mode-line-format 'none)
(setq treemacs-collapse-dirs 3)
(setq treemacs-indentation 1)
(setq treemacs-is-never-other-window nil)
(setq treemacs-position 'right)
(setq treemacs-width 35)
(after! treemacs
  ;; too slow on TRAMP connections
  (setq treemacs-eldoc-display 'nil)
  (treemacs-git-mode -1)
  (treemacs-filewatch-mode -1)

  ;; Only show a single project at a time (current project)
  (treemacs-project-follow-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lang/org
;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory (expand-file-name "~/Dropbox/orgs/"))
(setq +org-dir (expand-file-name "~/Dropbox/orgs/"))
(setq +org-export-directory (expand-file-name "~/Dropbox/orgs/.export"))
(setq org-attach-id-dir (expand-file-name "~/Dropbox/orgs/.attach"))
(setq org-download-image-dir (expand-file-name "~/Dropbox/orgs/.attach"))
(setq org-roam-directory "~/Dropbox/orgs/mywiki")
(setq org-roam-dailies-directory "~/Dropbox/orgs/journal")

;; org-journal
(setq org-journal-file-type 'weekly)
(setq org-journal-carryover-items t)
(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-dir "~/Dropbox/orgs/journal")
(setq +org-journal-common-headers "#+STARTUP: showall
#+STARTUP: inlineimages
#+STARTUP: logreschedule
#+STARTUP: logdone
#+STARTUP: logrefile
#+STARTUP: logdeadline


")
(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (format-time-string (pcase org-journal-file-type
                         (`daily (concat "#+TITLE: Daily Journal (%Y%m%d)\n" +org-journal-common-headers))
                         (`weekly (concat "#+TITLE: Weekly Journal (%Y%m%d)\n" +org-journal-common-headers))
                         (`monthly (concat "#+TITLE: Monthly Journal (%Y%m)\n" +org-journal-common-headers))
                         (`yearly (concat "#+TITLE: Yearly Journal (%Y)\n" +org-journal-common-headers)))
                       (org-journal--convert-time-to-file-type-time time))))
(setq org-journal-file-header 'org-journal-file-header-func)
(setq org-journal-enable-agenda-integration t)

(setq org-ellipsis " >")
(setq org-id-link-to-org-use-id t)

;; eldoc in org-mode src blocks recurses https://github.com/hlissner/doom-emacs/issues/2972
(after! org-eldoc
  (puthash "cpp" #'ignore org-eldoc-local-functions-cache)
  (puthash "python" #'ignore org-eldoc-local-functions-cache))

(after! org
  (setq org-id-link-to-org-use-id t)

  ;; scrolling in large org files is too slow if enabled
  (setq org-highlight-latex-and-related nil)

  ;; for performance
  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#why-is-scrolling-slow-in-emacsdoom
  (remove-hook 'org-mode-hook #'org-superstar-mode)
  ;; (setq org-fontify-quote-and-verse-blocks nil)
  ;; (setq org-fontify-whole-heading-line nil)
  ;; (setq org-hide-leading-stars nil)
  ;; (setq org-startup-indented nil)

  ;; (setq org-image-actual-width (/ (display-pixel-width) 3))
  (setq org-image-actual-width 400)

  (setq-default org-cycle-separator-lines 0)
  (setq-default org-agenda-inhibit-startup nil)

  ;; https://github.com/hlissner/doom-emacs/issues/3085
  (setq org-id-link-to-org-use-id 'use-existing)

  ;; html export
  (setq-default org-html-htmlize-output-type 'css)
  (setq org-export-use-babel nil)
  ;; From http://dakrone.github.io/org.css
  (setq-default org-html-head "<link rel=\"stylesheet\" href=\"/styles/styles.css\" type=\"text/css\" />")

  ;; use python3 in org-babel
  (setq org-babel-python-command "python3")


  (setq org-agenda-files (list
                          +org-dir
                          (expand-file-name "projects-gtd" +org-dir)
                          (expand-file-name "journal" +org-dir)))

  (setq org-publish-project-alist
        '(("org-docs"
           :base-directory "~/Dropbox/orgs"
           :base-extension "org"
           :publishing-directory "~/Dropbox/orgs_published/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t)
          ("org-static"
           :base-directory "~/Dropbox/orgs/publish_static"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/Dropbox/orgs_published/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org-attachments"
           :base-directory "~/Dropbox/orgs/.attach"
           :publishing-directory "~/Dropbox/orgs_published/.attach"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org-all"
           :components ("org-docs" "org-static" "org-attachments"))))

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
    '(("^\\*Org-Drill\\*$" :side bottom :size 3 :select t :quit nil :ttl nil)))

  (map! :map org-drill-response-mode-map
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
  :hook (org-mode . org-super-agenda-mode)
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-span 'day
        org-agenda-start-on-weekday nil
        org-agenda-start-with-log-mode t)

  ;; Make evil keybindings in org-super-agenda headers
  ;; https://github.com/alphapapa/org-super-agenda/issues/50#issuecomment-817432643
  (after! evil-org-agenda
    (setq org-super-agenda-header-map evil-org-agenda-mode-map))

  (setq org-super-agenda-groups
        '((:name "Today"
           :time-grid t
           :date today
           :todo "TODAY"
           :scheduled today
           :order 1)
          (:log t)
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
          (:discard (:not (:todo "TODO"))))))


;; org-modern
;; https://github.com/minad/org-modern
(use-package! org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :init
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"))


;; elfeed
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

(after! deft
  (add-to-list 'deft-extensions "md")
  (setq deft-recursive t)
  (setq deft-directory "~/Dropbox/orgs")
  (setq deft-archive-directory "~/Dropbox/orgs")

  ;; TODO use doom-escape-hook
  ;; (map! :map deft-mode-map
  ;;       [escape] #'quit-window)
  )

(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

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
(use-package! magit-delta
 :after magit
 :config
 (magit-delta-mode +1))
(after! magit
  (when IS-MAC
    (setq magit-git-executable "/usr/local/bin/git"))

  ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

  )

;; (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
;; (after! forge
;;   ;; indeed
;;   (add-to-list 'forge-alist '("code.corp.indeed.com" "code.corp.indeed.com/api/v4" "code.corp.indeed.com" forge-gitlab-repository)))


;; Completion

;; Corfu (vertico equivalent for company)
;; See modules/completion/corfu

;;
;; help
;;

;; enable word-wrap in help buffers
;; visual-linde-mode is nil
;;(unless EMACS29+
;;  (add-hook! help-mode visual-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp customizations
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq lsp-auto-configure t)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-modeline-diagnostics-enable nil)   ;; we already have flycheck in modeline
(setq lsp-modeline-diagnostics-scope :file)
(setq lsp-enable-dap-auto-configure t)
(setq lsp-lens-enable t)
(setq lsp-enable-semantic-highlighting nil)
(setq lsp-enable-links t)
(setq lsp-headerline-breadcrumb-segments '(file symbols))
(setq lsp-diagnostics-provider :flycheck)

;; for performance
;; (setq lsp-ui-sideline-enable nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-log-io nil)
(setq lsp-idle-delay 0.3)
;; (setq lsp-ui-sideline-delay 1.0)
(setq lsp-print-performance nil)

(setq lsp-treemacs-theme "Idea")
(setq lsp-server-install-dir "/Users/arezai/bin/lsp")

;; Currently ccls-code-lens-mode is enabled in all lsp languages
;; which causes multiple code actions to appear in some langauges (i.e rust)
;; However the ccls implementation is not eligible with other langs
;; Until this is fixed: https://github.com/hlissner/doom-emacs/pull/4233
(after! lsp-mode
  (remove-hook 'lsp-lens-mode-hook #'ccls-code-lens-mode)
  (add-hook! lsp-mode
             (defun +arif/remove-ccls-lens-mode ()
               "Deactivate ccls-lens-mode from lsp-mode"
               (when ccls-code-lens-mode
                 (ccls-code-lens-mode -1)))))

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

  ;; Disabled by default, each project should set it in .dir-locals
  (setq lsp-java-boot-enabled nil)
  (require 'lsp-java-boot)
  (define-minor-mode +lsp-java-boot-lens-mode
    "Toggle code-lens overlays for bootls conditionally."
    :group 'lsp-java-boot
    :global nil
    :init-value nil
    :lighter "+BLens"
    (cond
     (+lsp-java-boot-lens-mode
      (when lsp-java-boot-enabled
        (lsp-java-boot-lens-mode +lsp-java-boot-lens-mode)))
     (lsp-java-boot-lens-mode
      (lsp-java-boot-lens-mode +lsp-java-boot-lens-mode))))
  (add-hook 'java-mode-hook #'+lsp-java-boot-lens-mode)

  (setq lsp-java-configuration-runtimes
        (lsp-ht ("name" "java_home")
                ("default" t)
                ("path" (getenv "JAVA_HOME"))
                ("sources" (concat (getenv "JAVA_HOME") "/src.zip"))))
  (setq lsp-java-import-gradle-java-home (getenv "JAVA_HOME"))

  (setq lsp-java-workspace-dir (expand-file-name "~/.lsp/java-workspace")
        lsp-java-workspace-cache-dir (expand-file-name ".cache" lsp-java-workspace-dir))

  ;; Slightly better defaults
  (setq lsp-java-vmargs '("-noverify"
                          "-XX:+UseG1GC"
                          "-XX:+UseStringDeduplication"
                          "-Dsun.zip.disableMemoryMapping=true"
                          "-Xmx3G"
                          "-Xms256m"))

  ;; decompiler
  (setq lsp-java-content-provider-preferred "fernflower")

  ;; lsp-java requires JDK11
  (setq lsp-java-java-path (concat (string-trim-right (shell-command-to-string "/usr/libexec/java_home -v 11")) "/bin/java")
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
        lsp-java-inhibit-message t))


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

  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)

  ;; additional clients
  (require 'lsp-pyright)
  (require 'lsp-java)
  (require 'lsp-go)
  (require 'lsp-metals)
  (require 'lsp-javascript)
  (require 'lsp-haskell)
  (require 'lsp-rust)

  ;;(defun lsp-register-remote-client (based-on &rest args)
  ;;  ;; Create a copy of based-on client
  ;;  (let ((client (copy-lsp--client (gethash based-on lsp-clients))))
  ;;    ;; Set a default server id and set remote
  ;;    (setf (lsp--client-server-id client) (make-symbol (format "%s-remote" based-on))
  ;;          (lsp--client-remote? client) t)
  ;;    (cl-loop for (prop val) on args by 'cddr do
  ;;             (let* ((slot-sym (intern (substring (symbol-name prop) 1)))
  ;;                    (slot (ignore-error 'cl-struct-unknown-slot
  ;;                            (cl-struct-slot-value 'lsp--client slot-sym client))))
  ;;               (when slot
  ;;                 (setf slot val))))
  ;;    (lsp-register-client client)))

  ;; not working currently, need to debug
  ;;(lsp-register-remote-client
  ;; 'ts-ls
  ;; :new-connection (lsp-tramp-connection '("typescript-language-server" "--stdio"))
  ;; :major-modes '(js-mode typescript-mode typescript-tsx-mode))

  (set-popup-rules!
    '(("^\\*lsp-help\\*" :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :select t :quit t :ttl 0))))

;; lsp-metals
(setq lsp-metals-server-command "metals-emacs")


;; Automatically call dap-hydra when execution stopped
(after! dap-mode
  (dap-auto-configure-mode 1)
  (add-hook 'dap-stopped-hook
          (lambda (_arg) (call-interactively #'dap-hydra))))

(after! lsp-ui
  ;; default lsp disables it
  (setq lsp-ui-doc-enable t)

  ;; only show documentation with mouse
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)

  (setq
   lsp-ui-doc-use-webkit t        ;; webkit renders badly for some langs
   lsp-ui-doc-position 'at-point

   lsp-ui-doc-delay 0.3
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-max-height 60
   lsp-ui-doc-max-width 120)

  ;; lsp-ui-sideline
  (setq
   lsp-ui-sideline-enable t
   lsp-ui-sideline-delay 0.5
   lsp-ui-sideline-show-diagnostics nil
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-ignore-duplicate t)

  ;; from lsp-java
  ;; lsp-ui does not display all of the actions on the current point (e. g "Extract constant")?
  ;; LSP UI by default sends current line bounds for action region which breaks,
  ;; and forces JDT server to return only "Extract method action."
  (setq lsp-ui-sideline-update-mode 'point))

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


  ;; (set-popup-rules!
  ;;   '(("^\\*Flycheck errors\\*"
  ;;      :modeline nil :select nil :quit current
  ;;      :side bottom :slot 9999 :vslot 9999)))

  )

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

;; minizinc
;; (require 'minizinc-mode)
;; (add-to-list 'auto-mode-alist '("\\.mzn\\'" . minizinc-mode))

;; (after! proof-general
;;   (require 'coq-inferior))

;; (after! org-src
;;   ;; ~/.doom.d/local/ob-minizinc.el
;;   (require 'ob-minizinc))

;; Haskell
(after! haskell
  (after! lsp-mode
    ;; will define elisp functions for the given lsp code actions, prefixing the
    ;; given function names with "lsp"
    (lsp-make-interactive-code-action wingman-fill-hole "refactor.wingman.fillHole")
    (lsp-make-interactive-code-action wingman-case-split "refactor.wingman.caseSplit")
    (lsp-make-interactive-code-action wingman-refine "refactor.wingman.refine")
    (lsp-make-interactive-code-action wingman-split-func-args "refactor.wingman.spltFuncArgs")
    (lsp-make-interactive-code-action wingman-use-constructor "refactor.wingman.useConstructor")

    ;; example key bindings
    (define-key haskell-mode-map (kbd "C-c d") #'lsp-wingman-case-split)
    (define-key haskell-mode-map (kbd "C-c n") #'lsp-wingman-fill-hole)
    (define-key haskell-mode-map (kbd "C-c r") #'lsp-wingman-refine)
    (define-key haskell-mode-map (kbd "C-c c") #'lsp-wingman-use-constructor)
    (define-key haskell-mode-map (kbd "C-c a") #'lsp-wingman-split-func-args))

  ;; Disable as it is annoying when using DataKinds
  (after! smartparens
    (sp-local-pair 'haskell-mode "'" "'" :actions nil))

  (setq haskell-process-log t)
  (setq haskell-interactive-popup-errors nil)

  (set-popup-rules!
    '(("^\\*HS-Error\\*" :slot -1 :vslot -1 :size 10 :select nil :quit t :ttl 0))))

;; themes
;;(use-package modus-themes
;;  :demand t
;;  :init
;;  ;; Load the theme files before enabling a theme
;;  (modus-themes-load-themes)
;;  :config
;;  ;; Load the theme of your choice:
;;  (modus-themes-load-vivendi))
;;  ;;(modus-themes-load-operandi))

;; doom-theme
(setq doom-theme 'doom-gruvbox-light)
;; (setq doom-theme 'modus-operandi)
;; (setq doom-theme 'doom-solarized-light)

;; disable smartparens, scrolling large org files is very slow
;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; file-templates
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

(use-package! devdocs-browser
  :commands (devdocs-browser-open
             devdocs-browser-open-in
             devdocs-browser-list-docs
             devdocs-browser-update-docs
             devdocs-browser-install-doc
             devdocs-browser-uninstall-doc
             devdocs-browser-upgrade-doc
             devdocs-browser-download-offline-data
             devdocs-browser-remove-offline-data))

;; ws-butler
;; Trim extra whitespace from modified lines
(setq ws-butler-keep-whitespace-before-point nil)

;; safe vars
(put 'lsp-java-vmargs 'safe-local-variable (lambda (_) t))

;; https://github.com/hlissner/doom-emacs-private/blob/master/config.el
(after! emacs-everywhere
  ;; arif: Not on linux, but good to have nevertheless
  ;; Easier to match with a bspwm rule:
  ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")

  ;; The modeline is not useful to me in the popup window. It looks much nicer
  ;; to hide it.
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode))

(load! "indeed")
