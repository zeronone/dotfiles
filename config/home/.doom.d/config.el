;;; config.el -*- lexical-binding: t; -*-

;; (setq comp-deferred-compilation t)

;;;
;;; Eager load
;;;

(add-to-list 'load-path "~/.doom.d/local")

;; improve startup time
;; (after! gcmh
;;   (setq gcmh-high-cons-threshold (* 128 1024 1024)))
(setq inhibit-compacting-font-caches t)

;; load all packages when in deamon mode
(setq use-package-always-demand (daemonp))

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'meta
      x-alt-keysym   'alt)

;; no fringes
;; (set-fringe-mode '(0 . 0))

;; Initial frame size
(when window-system (set-frame-size (selected-frame) 150 50))

;; Fancy look
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Disable visual line mode
(visual-line-mode -1)
(setq truncate-lines nil)

;; default indent
(setq-default tab-width 4)

(setq straight-vc-git-default-clone-depth 10)

(setq-default
 user-full-name    "Arif Rezai"
 user-mail-address "me@arifrezai.com")

(use-package! direnv
  :when (executable-find "direnv")
  :demand t
  :mode ("\\.envrc\\'" . +direnv-rc-mode)
  :config
  (direnv-mode +1))

;; which-key
(after! which-key
  (which-key-setup-side-window-right-bottom))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; (setq doom-font (font-spec :family "Input Mono Narrow" :size 14)
;;       doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
;;       doom-big-font (font-spec :family "Fira Mono" :size 19))

(setq doom-font (font-spec :family "Hack" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
      doom-big-font (font-spec :family "Hack" :size 19))

;; Fira Mono doesn't have italics, so we highlight it instead.
(add-hook! doom-post-init
  (set-face-attribute 'italic nil :weight 'ultra-light :foreground "#ffffff"))

(add-hook! doom-big-font-mode
  (setq +doom-modeline-height (if doom-big-font-mode 37 29)))

;; doom-modeline is disabled currently
;; doom-modeline
;; (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
;; (setq doom-modeline-persp-name t)
;; (setq show-trailing-whitespace t)
;; (setq doom-modeline-buffer-state-icon nil)
;; (after! doom-modeline
;;  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode) ; filesize in modeline
;;  (line-number-mode -1))

;; (setq-default +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! '(minibuffer-setup-hook doom-popup-mode-hook)
  (setq-local show-trailing-whitespace nil))

;; From hlissner private config
(setq
 ;; Line numbers are pretty slow all around. The performance boost of
 ;; disabling them outweighs the utility of always keeping them on.
 display-line-numbers-type nil

)

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
 :n "C-l"   #'evil-window-right

 (:after treemacs-evil
   (:map evil-treemacs-state-map
     "C-h" #'evil-window-left
     "C-l" #'evil-window-right))

 (:leader
   (:prefix "g"
     :desc "Magit branches"        :n "B" #'magit-branch-popup)))

(setq treemacs-git-mode nil)
(setq +treemacs-git-mode nil)
;; treemacs
(after! treemacs-evil
  (define-key! treemacs-mode-map
    "h" nil
    "l" nil)

  (evil-define-key 'treemacs treemacs-mode-map (kbd "h") nil)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "l") nil)

  (define-key! evil-treemacs-state-map
    "h" nil
    "l" nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lang/org
;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory (expand-file-name "~/Dropbox/orgs/"))
(setq +org-dir (expand-file-name "~/Dropbox/orgs/"))

(setq +org-export-directory (expand-file-name "~/Dropbox/orgs/.export"))

(setq org-attach-id-dir (expand-file-name "~/Dropbox/orgs/.attach"))
(setq org-download-image-dir (expand-file-name "~/Dropbox/orgs/.attach"))

(setq org-roam-directory "~/Dropbox/orgs/mywiki")

(setq org-journal-file-format "%Y%m%d.org")
(setq org-journal-dir "~/Dropbox/orgs/journal")
(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal (%Y%m%d)\n#+STARTUP: showall\n")
      (`weekly "#+TITLE: Weekly Journal (%Y%m%d)\n#+STARTUP: showall\n")
      (`monthly "#+TITLE: Monthly Journal (%Y%m)\n#+STARTUP: folded\n")
      (`yearly "#+TITLE: Yearly Journal (%Y)\n#+STARTUP: folded\n"))))
(setq org-journal-file-header 'org-journal-file-header-func)

(setq org-noter-notes-search-path "~/Dropbox/orgs/mywiki/notes")

(setq org-pdftools-search-string-separator "??")
(setq org-ellipsis " ▼ ")
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
  (setq org-agenda-files (directory-files-recursively +org-dir "\\.org$"))
  ;; (setq org-agenda-files (list "~/Dropbox/orgs"
  ;;                              "~/Dropbox/orgs/archive"
  ;;                              "~/Dropbox/orgs/journal"
  ;;                              "~/Dropbox/orgs/mywiki"
  ;;                              "~/Dropbox/orgs/line"
  ;;                              "~/Dropbox/orgs/verda"
  ;;                              "~/Dropbox/orgs/toptal"))

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


  (setq org-refile-additional-targets-a '("~/Dropbox/orgs/verda"))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-refile-additional-targets-a :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t))

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

;; elfeed
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

(use-package! hydra
  :demand t)

(after! deft
  (add-to-list 'deft-extensions "md")
  (setq deft-recursive t)
  (setq deft-directory "~/Dropbox/orgs")
  (setq deft-archive-directory "~/Dropbox/orgs")

  (map! :map deft-mode-map
        [escape] #'quit-window))

;; org-journal
(after! org-journal
  (setq org-journal-file-type 'weekly)
  (setq org-journal-carryover-items t)

  (setq org-journal-enable-agenda-integration t
        org-icalendar-store-UID t
        org-icalendar-include-todo "all"
        org-icalendar-combined-agenda-file "~/Dropbox/orgs/org-journal.ics"))

(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (advice-add 'pdf-view-mouse-set-region :override #'*pdf-view-mouse-set-region))
(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))
(use-package! org-noter-pdftools
  :config
  (after! pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
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

;; tabnine
(use-package! company-tabnine)
(use-package! company-try-hard
  :config

  (global-set-key (kbd "C-SPC") #'company-try-hard)
  (define-key company-active-map (kbd "C-SPC") #'company-try-hard))

;; company
(after! company
  ;;  original: (not erc-mode message-mode help-mode gud-mode eshell-mode)
  (setq company-global-modes '(not org-mode erc-mode message-mode help-mode gud-mode eshell-mode))
  (setq company-idle-delay 0.3)

  ;; defaults
  (setq company-backends '(company-capf company-tabnine)))

(after! company-box
  ;; trigger manually with C-h when completion box  is open
  (setq company-box-doc-enable nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp customizations
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq lsp-auto-configure t)
(setq lsp-ui-doc-enable t)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-enable-dap-auto-configure t)
(setq lsp-lens-enable t)
(setq lsp-completion-provider :capf)
(setq lsp-enable-semantic-highlighting t)
(setq lsp-enable-links t)
(setq lsp-headerline-breadcrumb-segments '(file symbols))
(setq lsp-diagnostics-provider :flycheck)

;; for performance
(setq lsp-enable-file-watchers t)
(setq lsp-ui-sideline-enable nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-log-io nil)

(use-package! lsp-pyright)

(after! lsp-mode

  ;; Workaround for issue #3274
  (setq-hook! '(lsp-managed-mode-hook)
    flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))

  ;; clangd
  (setq lsp-clients-clangd-args '("--compile-commands-dir=build"
                                  "--pch-storage=memory"
                                  "--background-index"
                                  "-j=4"))

  (require 'lsp-treemacs)
  (lsp-treemacs-sync-mode 1)

  ;; additional clients
  (require 'lsp-pyright)

  (set-popup-rules!
    '(("^\\*lsp-help\\*" :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :select t :quit t :ttl 0))))

(after! lsp-clients
  (set-lsp-priority! 'clangd 1))

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

   lsp-ui-doc-delay 3.0
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

;; Disable title bars
;; (setq default-frame-alist '((undecorated . t)))

;; format module is disabled
;; (setq-default +format-on-save-enabled-modes '(not emacs-lisp-mode rjsx-mode javascript-mode))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(package-lint)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package! package-lint)
