;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; Experimental, remove once Doom Emacs has support for corfu
;; Taken/modified from: https://git.sr.ht/~gagbo/doom-config/tree/master/item/modules/completion/corfu/config.el

(use-package! compat
  :demand t)

(setq tab-always-indent 'complete)
(use-package! corfu
  :hook (doom-first-buffer . global-corfu-mode)
  :init
  (setq corfu-auto nil)               ;; Enable auto completion

  ;; Enable the tab-and-go style, rather than TAB selecting the current candidate
  (setq corfu-preselect 'prompt)
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'

  ;; enable poupinfo
  (setq corfu-popupinfo-delay 0.2)
  (corfu-popupinfo-mode +1)

  ;; Tab-and-go options
  (setq corfu-separator ?\s)          ;; Orderless field separator
  (setq corfu-quit-no-match 'separator)
  (setq corfu-quit-at-boundary 'separator)

  (after! lsp-mode
    (setq lsp-completion-provider :none)

    ;; details while completion
    (setq lsp-completion-show-detail nil)
    (setq lsp-completion-show-kind nil)

    (add-hook! 'lsp-completion-mode-hook
      (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless))))))

  :config
  (defun +arif/corfu-quit-completion-in-region ()
    "Same as implementation of corfu-quit"
    (when (bound-and-true-p completion-in-region-mode)
        (completion-in-region-mode -1)
        t))
  (add-hook! 'doom-escape-hook :depth -100 #'+arif/corfu-quit-completion-in-region)

  ;; keymaps
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (map! "C-SPC" #'completion-at-point
        :map corfu-map
        "C-SPC" #'corfu-insert-separator
        "TAB" #'corfu-next
        [tab] #'corfu-next
        "S-TAB" #'corfu-previous
        [backtab] #'corfu-previous
        "M-m" #'corfu-move-to-minibuffer
        [escape] #'doom/escape)

  )


;; Credits: https://github.com/XzoRit/home/blob/21463806fec8161251471c3f5d63d43b56a6ef95/.emacs
(use-package! cape
  :demand t
  :init
  (defhydra +arif:hydra-cape (:color red
                             :hint nil
                             :pre (corfu-quit)
                             :exit t)
    "
cape:
    _k_: cape-keyword _p_: completion-at-point _t_: complete-tag
    _a_: cape-abbrev  _d_: cape-dabbrev
    _l_: cape-line    _f_: cape-file
    _r_: cape-rfc1345 _s_: cape-symbol
    _w_: cape-dict    _i_: cape-ispell
    _&_: cape-sgml    _\\_: cape-tex
"
    ("p" completion-at-point)
    ("t" complete-tag)
    ("d" cape-dabbrev)
    ("f" cape-file)
    ("k" cape-keyword)
    ("s" cape-symbol)
    ("a" cape-abbrev)
    ("i" cape-ispell)
    ("l" cape-line)
    ("w" cape-dict)
    ("\\" cape-tex)
    ("&" cape-sgml)
    ("r" cape-rfc1345))
  (map! :g "C-c i" #'+arif:hydra-cape/body)

  ;; Enable the followings
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; (use-package! citre
;;   :hook (prog-mode . citre-mode)
;;   :init
;;   ;; This is needed in `:init' block for lazy load to work.
;;   (require 'citre-config)
;;   (setq citre-enable-capf-integration t)
;;   (setq citre-enable-imenu-integration t)

;;   :config
;;   (setq citre-use-project-root-when-creating-tags t)
;;   (setq citre-project-root-function #'projectile-project-root)
;;   (setq citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package! kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
