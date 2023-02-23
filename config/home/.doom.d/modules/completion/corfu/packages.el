;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! company :disable t)
(package! corfu
	  :pin "b5458a132c678b5fe97b4a7819b9bb1dba31aee2"
	  :recipe (:host github :repo "minad/corfu"))
(package! cape)
(package! kind-icon)
;; (package! citre)

(package! compat :recipe (:host github :repo "emacs-compat/compat"))




