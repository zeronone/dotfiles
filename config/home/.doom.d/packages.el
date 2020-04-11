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
(package! org-drill)

(package! org-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("org-pdftools.el")))
(package! org-noter-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("org-noter-pdftools.el")))
(package! org-noter)

;; anzu.vim
(package! anzu)
(package! evil-anzu)

;; minizinc
;; (package! minizinc-mode :load-path "~/.doom.d/local/minizinc-mode.el")
