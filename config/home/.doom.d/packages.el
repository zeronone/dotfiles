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
(package! modus-themes)
(package! darktooth-theme)

;; hydra
(package! pretty-hydra)
(package! major-mode-hydra)

;; s for string modifications
(package! s)

;; dash
(package! dash)

;; by alphapapa
(package! org-web-tools :recipe (:host github :repo "zeronone/org-web-tools"))
(package! org-super-agenda)
(package! org-sidebar :recipe (:host github :repo "alphapapa/org-sidebar"))
(package! org-ql)

;; SRS in org
(package! org-drill :recipe (:host github :repo "zeronone/org-drill"))

;; org-modern
(package! org-modern)

;; oj
(package! oj)

;; disable  pipenv
(package! pipenv :disable t)

;; package-lint
(package! package-lint)

;; lsp
(package! lsp-pyright :recipe (:host github :repo "emacs-lsp/lsp-pyright"))

;; evil
(package! evil-motion-trainer :recipe (:host github :repo "martinbaillie/evil-motion-trainer"))

;; devdocs
(package! devdocs-browser)

;; magit
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))

;; unpins
;; (unpin! dap-mode lsp-mode lsp-pyright lsp-java lsp-ui rustic org-roam)
