;;; config.el -*- lexical-binding: t; -*-

(setq module-file-suffix ".so")

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'meta
      x-alt-keysym   'alt)

;; Initial frame size
(when window-system (set-frame-size (selected-frame) 150 50))

;; Fancy look
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Disable visual line mode
(visual-line-mode -1)
(setq truncate-lines nil)

;; default indent
(setq-default tab-width 2)

(setq straight-vc-git-default-clone-depth 10)

(setq-default
 user-full-name    "Arif Rezai"
 user-mail-address "me@arifrezai.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

(setq doom-font (font-spec :family "Input Mono Narrow" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
      doom-big-font (font-spec :family "Fira Mono" :size 19))

;; Fira Mono doesn't have italics, so we highlight it instead.
(add-hook! doom-post-init
  (set-face-attribute 'italic nil :weight 'ultra-light :foreground "#ffffff"))

(add-hook! doom-big-font-mode
  (setq +doom-modeline-height (if doom-big-font-mode 37 29)))

;; doom-theme
;; (setq doom-theme 'wombat)
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-Iosvkem)

;; doom-modeline
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq doom-modeline-persp-name t)
(setq show-trailing-whitespace t)

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
(setq org-roam-directory "~/Dropbox/orgs")
(setq org-journal-dir "~/Dropbox/orgs")
(setq org-noter-notes-search-path "~/Dropbox/orgs")
(setq org-pdftools-search-string-separator "??")
(setq org-ellipsis " ▼ ")
(setq org-id-link-to-org-use-id t)

(after! org

  (setq org-image-actual-width (/ (display-pixel-width) 3))

  (setq-default org-cycle-separator-lines 0)
  (setq-default org-agenda-inhibit-startup nil)

  ;; html export
  (setq-default org-html-htmlize-output-type 'css)
  (setq-default org-html-head "<link rel=\"stylesheet\" href=\"http://dakrone.github.io/org.css\" type=\"text/css\" />")

  ;; use python3 in org-babel
  (setq org-babel-python-command "python3")

  (setq org-image-actual-width 400)
  (setq org-agenda-files (list "~/Dropbox/orgs/"
                               "~/Dropbox/orgs/journal"
                               "~/Dropbox/orgs/personal-wiki"
                               "~/Dropbox/orgs/line-wiki"
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
        "C-c c" #'org-drill-response-rtn)

  (setq org-drill-presentation-prompt-with-typing t)
  (setq org-drill-left-cloze-delimiter "[hint][")
  (setq org-drill-maximum-items-per-session 60)
  (setq org-drill-maximum-duration 90)           ; 90 minutes
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t))

(after! deft
  (add-to-list 'deft-extensions "md")
  (setq deft-recursive t)
  (setq deft-directory "~/Dropbox/orgs")
  (setq deft-archive-directory "~/Dropbox/orgs"))

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

(use-package! eterm-256color
  :hook (term-mode-hook . eterm-256color-mode))

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
  (setq company-idle-delay 0.7))

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
(after! rustic
  (set-evil-initial-state! 'rustic-popup-mode 'emacs)
  (set-popup-rules!
    '(("^rustic-popup-buffer" :select t :quit t :side bottom :slot 1)
      ("^\\*rustic-compilation\\*" :select nil :quit t :side bottom :slot 2)))

  ;; 'rust-analyzer is not ready yet
  (setq rustic-lsp-server 'rls))

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
