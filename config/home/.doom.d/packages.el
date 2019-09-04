;; -*- no-byte-compile: t; -*-
;;; packages.el

(package! org-wiki :recipe (:host github :repo "caiorss/org-wiki"))

;; (package! org-jira)

;; don't use exec-path-from-shell
;; Use osxwithsync/osxdosync
(package! exec-path-from-shell :disable t)

;; Use Microsoft python language server
(package! lsp-python-ms
          :recipe (:host github :repo "andrew-christianson/lsp-python-ms"))

;; use pycheckers
(package! flycheck-pycheckers)


;; disable flycheck UI
(package! flycheck-popup-tip :disable t)
(package! flycheck-posframe :disable t)

;; dap mode
(package! dap-mode)
