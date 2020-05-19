;;; config.el -*- lexical-binding: t; -*-

;; improve startup time
(after! gcmh
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))
(setq inhibit-compacting-font-caches t)

;; load all packages when in deamon mode
(setq use-package-always-demand (daemonp))

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'meta
      x-alt-keysym   'alt)

;; no fringes
(set-fringe-mode '(0 . 0))

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

;; which-key
(after! which-key
  (which-key-setup-side-window-right-bottom))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

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

 ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
 ;; disable it by default.
 lsp-ui-sideline-enable nil
 lsp-enable-indentation nil
 ;; lsp-enable-on-type-formatting nil
 ;; lsp-enable-symbol-highlighting nil
 ;; lsp-enable-file-watchers nil
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


;; Org default directory
(setq org-directory (expand-file-name "~/Dropbox/orgs/"))
(setq +org-dir (expand-file-name "~/Dropbox/orgs/"))
(setq +org-export-directory (expand-file-name "~/Dropbox/orgs/.export"))
(setq org-attach-id-dir (expand-file-name "~/Dropbox/orgs/.attach"))
(setq org-download-image-dir (expand-file-name "~/Dropbox/orgs/.attach"))
(setq org-roam-directory "~/Dropbox/orgs/mywiki")
(setq org-journal-dir "~/Dropbox/orgs/mywiki/journal")
(setq org-noter-notes-search-path "~/Dropbox/orgs/mywiki/notes")
(setq org-pdftools-search-string-separator "??")
(setq org-ellipsis " ▼ ")
(setq org-id-link-to-org-use-id t)

;; eldoc in org-mode src blocks recurses https://github.com/hlissner/doom-emacs/issues/2972
(after! org-eldoc
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

  ;; This should be put here rather than in (after! org-journal)
  (defun +org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  ;; org-capture-templates
  (add-to-list 'org-capture-templates
               '("j" "Journal entry" entry (function +org-journal-find-location)
                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?" :prepend t))

  (setq org-image-actual-width 400)
  (setq org-agenda-files (list "~/Dropbox/orgs/"
                               "~/Dropbox/orgs/journal"
                               "~/Dropbox/orgs/mywiki"
                               "~/Dropbox/orgs/line"
                               "~/Dropbox/orgs/verda"
                               "~/Dropbox/orgs/toptal"))

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

(use-package! hydra
  :ensure t)

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

(use-package! org-noter-pdftools
  :after org-noter
  (after! pdf-tools
    (setq pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

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

;; company
(after! company
  ;;  original: (not erc-mode message-mode help-mode gud-mode eshell-mode)
  (setq company-global-modes '(not org-mode erc-mode message-mode help-mode gud-mode eshell-mode))
  (setq company-idle-delay 0.2))

(after! company-box
  ;; trigger manually with C-h when completion box  is open
  (setq company-box-doc-enable nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp customizations
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(after! company-lsp
  (setq company-lsp-async t)
  (setq company-lsp-cache-candidates 'auto))

(after! lsp-mode
  (setq
   ;; auto configure lsp-ui, lsp-company ...
   lsp-auto-configure t
   lsp-auto-require-clients nil

   ;; https://github.com/hlissner/doom-emacs/issues/2060#issuecomment-554165917
   ;; lsp-prefer-flymake nil   ;; deprecated
   lsp-diagnostic-package :flycheck

   ;; for performance
   lsp-log-io nil)

  (set-popup-rules!
    '(("^\\*lsp-help\\*" :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :select t :quit t :ttl 0))))

;; Automatically call dap-hydra when execution stopped
(after! dap-mode
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra))))

(after! lsp-ui
  (setq
   ;; disable doc-mode
   ;; lsp-ui-doc-enable nil
   ;; the max-width doesn't work for webkit
   ;; lsp-ui-doc-use-webkit t
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-alignment 'frame
   ;; avoid documentation being rednered as markdown
   lsp-ui-doc-render-function 'nil
   lsp-ui-doc-delay 0.5
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-max-height 45
   lsp-ui-doc-max-width 90
   lsp-ui-doc-position 'bottom

   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t

   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs))))

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

;; pyvenv fix
;; https://github.com/palantir/python-language-server/issues/431
;; (after! pyvenv
;;   (defun pyenv-venv-wrapper-act (&optional ARG PRED)
;;     (setenv "VIRTUAL_ENV" (shell-command-to-string "_pyenv_virtualenv_hook; echo -n $VIRTUAL_ENV")))
;;   (advice-add 'pyenv-mode-set :after 'pyenv-venv-wrapper-act)
;;   (defun pyenv-venv-wrapper-deact (&optional ARG PRED)
;;     (setenv "VIRTUAL_ENV"))
;;   (advice-add 'pyenv-mode-unset :after 'pyenv-venv-wrapper-deact))

;; vterm colors fix
;; From: https://github.com/akermu/emacs-libvterm/issues/73
;; (after! vterm
;;   (setq ansi-color-names-vector
;;         ["#202020" "#ff8272" "#b4fa72" "#fefdc2" "#a5d5fe" "#ff8ffd" "#d0d1fe" "#f1f1f1"]))


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

(add-to-list 'load-path "~/.doom.d/local")
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
        modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2)
  :ensure t)
(use-package! modus-vivendi-theme
  ;; disabled
  :init
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
        modus-vivendi-theme-distinct-org-blocks t
        modus-vivendi-theme-scale-1 1.05
        modus-vivendi-theme-scale-2 1.1
        modus-vivendi-theme-scale-3 1.15
        modus-vivendi-theme-scale-4 1.2)
  :ensure t)

;; doom-theme
(setq doom-theme 'modus-operandi)
;; (setq doom-theme 'modus-vivendi)


;; disable smartparens, scrolling large org files is very slow
;; smartparens is core package, so it should be disabled here
;; https://github.com/Fuco1/smartparens/issues/464
;; (after! smartparens
;;   (add-hook 'org-mode-hook #'turn-off-smartparens-mode)
;;   (sp-local-pair 'org-mode "*" nil))

;; Custom functions
(defun subtree-to-new-file ()
  "Sloppily assists in moving an org subtree to a new file, if base-directory is defined it will be placed there"
  (interactive)
  (setq header-text (org-entry-get nil "ITEM"))
  (org-copy-subtree nil t)
  (setq new-filename (concat (s-snake-case header-text) ".org"))
  (find-file-other-window (expand-file-name new-filename (or base-directory org-directory)))
  (erase-buffer)
  (insert (concat "#+TITLE: " header-text "\n"))
  (insert "\n")
  (org-paste-subtree)
  (delete-window))

;; file-templates
(use-package! s
  :ensure t)
(defvar +private-file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "The path to a directory of yasnippet folders to use for file templates.")
(after! yasnippets
  (add-to-list 'yas-snippet-dirs '+private-file-templates-dir 'append #'eq)
  (yas-reload-all))
