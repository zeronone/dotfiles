;; -*- no-byte-compile: t; -*-
;;; packages.el

;; don't use exec-path-from-shell
;; use doom patch-macosx
(package! exec-path-from-shell :disable t)

;; disable flycheck UI
(package! flycheck-popup-tip :disable t)
(package! flycheck-posframe :disable t)

;; minizinc
;; (package! minizinc-mode :load-path "~/.doom.d/local/minizinc-mode.el")

;; themes
(package! modus-operandi-theme)
(package! modus-vivendi-theme)
(package! darktooth-theme)

;; hydra
(package! pretty-hydra)
(package! major-mode-hydra)

;; s for string modifications
(package! s)

;; by alphapapa
(package! org-web-tools :recipe (:host github :repo "zeronone/org-web-tools"))
(package! org-super-agenda)
(package! org-sidebar :recipe (:host github :repo "alphapapa/org-sidebar"))

;; SRS in org
(package! org-drill :recipe (:host github :repo "zeronone/org-drill"))

(package! oj)

;; disable  pipenv
(package! pipenv :disable t)

;; tabnine completion
;;(package! company-tabnine)
(package! company-try-hard)

;; package-lint
(package! package-lint)

(package! lsp-pyright :recipe (:host github :repo "emacs-lsp/lsp-pyright"))
(package! direnv)

;; unpins
(unpin! dap-mode lsp-mode lsp-pyright lsp-java)
