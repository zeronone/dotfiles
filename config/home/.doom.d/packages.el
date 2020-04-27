;; -*- no-byte-compile: t; -*-
;;; packages.el

;; (package! org-jira)

;; don't use exec-path-from-shell
;; use doom patch-macosx
(package! exec-path-from-shell :disable t)

;; use pycheckers
;; (package! flycheck-pycheckers)

;; disable flycheck UI
(package! flycheck-popup-tip :disable t)
(package! flycheck-posframe :disable t)

;; SRS in org
(package! org-drill :recipe (:host github :repo "zeronone/org-drill"))
(package! org-noter-pdftools)

;; anzu.vim
(package! anzu)
(package! evil-anzu)

;; minizinc
;; (package! minizinc-mode :load-path "~/.doom.d/local/minizinc-mode.el")

;; themes
(package! modus-operandi-theme)
(package! modus-vivendi-theme)

;; disable org-superstar
(package! org-superstar :disable t)

;; temp
(package! lsp-mode :pin "9c1ab8adf1a")
