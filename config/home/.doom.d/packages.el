;; -*- no-byte-compile: t; -*-
;;; packages.el

(package! org-wiki :recipe (:host github :repo "caiorss/org-wiki"))

;; (package! org-jira)

;; don't use exec-path-from-shell
;; use doom patch-macosx
(package! exec-path-from-shell :disable t)

;; use pycheckers
(package! flycheck-pycheckers)


;; disable flycheck UI
(package! flycheck-popup-tip :disable t)
(package! flycheck-posframe :disable t)

;; dap mode
(package! dap-mode)

;; SRS in org
(package! org-drill)

;; anzu.vim
(package! anzu)
(package! evil-anzu)

