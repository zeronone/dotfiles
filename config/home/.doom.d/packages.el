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

;; disable org-superstar
;; instead use: (remove-hook 'org-mode-hook #'org-superstar-mode)
;; (package! org-superstar :disable t)

;; s for string modifications
(package! s)

;; org noter
(package! org-noter-pdftools)

;; SRS in org
(package! org-drill :recipe (:host github :repo "zeronone/org-drill"))
