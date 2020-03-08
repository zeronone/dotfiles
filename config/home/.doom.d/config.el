;;; config.el -*- lexical-binding: t; -*-

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
(setq doom-theme 'doom-one)

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
(defvar +org-dir (expand-file-name "~/Dropbox/orgs/"))
(setq-default +org-export-directory "~/Dropbox/orgs/.export")

(after! org
  (setq-default org-cycle-separator-lines 0)
  (setq-default org-agenda-inhibit-startup nil)

  ;; use python3 in org-babel
  (setq org-babel-python-command "python3")

  (setq org-image-actual-width 400)

  (setq org-directory "~/Dropbox/orgs/")
  (setq org-agenda-files (list "~/Dropbox/orgs/"
                               "~/Dropbox/orgs/personal-wiki"
                               "~/Dropbox/orgs/line-wiki"
                               "~/Dropbox/orgs/line"
                               "~/Dropbox/orgs/verda"
                               "~/Dropbox/orgs/toptal"))
  (setq org-refile-additional-targets-a '("~/Dropbox/orgs/verda"))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-refile-additional-targets-a :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t))

(use-package! org-drill
  :after org)

;; lang/org
(after! org-bullets
  ;; The standard unicode characters are usually misaligned depending on the
  ;; font. This bugs me. Personally, markdown #-marks for headlines are more
  ;; elegant, so we use those.
  (setq org-bullets-bullet-list '("#")))

;; Everywhere else, I have big displays and plenty of space, so use it!
(setq org-ellipsis " ▼ ")

;; private/org-wiki
(use-package! org-wiki
  :commands org-wiki-index
  :config
  (setq org-wiki-template
        "#+TITLE: %n
#+DESCRIPTION:
#+KEYWORDS:
#+STARTUP:  showeverything


- [[wiki:index][Index]]

- Related:

* %n
")
  (setq org-wiki-location-list '("~/Dropbox/orgs/personal-wiki" "~/Dropbox/orgs/line-wiki"))
  (setq org-wiki-location (car org-wiki-location-list)))

;; org-brain
(setq org-brain-path "~/Dropbox/orgs/brain")

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
  ;; disable for org-mode
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp customizations
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq +lsp-company-backend 'company-capf)
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
    '(("^\\*lsp-help*" :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :select t :quit t :ttl 0))))

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
(after! pyvenv
  (defun pyenv-venv-wrapper-act (&optional ARG PRED)
    (setenv "VIRTUAL_ENV" (shell-command-to-string "_pyenv_virtualenv_hook; echo -n $VIRTUAL_ENV")))
  (advice-add 'pyenv-mode-set :after 'pyenv-venv-wrapper-act)
  (defun pyenv-venv-wrapper-deact (&optional ARG PRED)
    (setenv "VIRTUAL_ENV"))
  (advice-add 'pyenv-mode-unset :after 'pyenv-venv-wrapper-deact))

;; Add mspyls to exec-path
(add-to-list 'exec-path (concat doom-etc-dir "mypyls"))

;; vterm colors fix
;; From: https://github.com/akermu/emacs-libvterm/issues/73
;; (after! vterm
;;   (setq ansi-color-names-vector
;;         ["#202020" "#ff8272" "#b4fa72" "#fefdc2" "#a5d5fe" "#ff8ffd" "#d0d1fe" "#f1f1f1"]))


;; disable flycheck on escape key
(setq +flycheck-on-escape nil)
(after! flycheck

  (setq flycheck-check-syntax-automatically '(save))

  (set-popup-rule! "^\\*Flycheck errors\\*"
    :modeline nil :select nil :quit 'current
    :side 'bottom :slot 9999 :vslot 9999))

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
