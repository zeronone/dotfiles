;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! company :disable t)
(package! corfu
          :recipe (:files (:defaults "extensions/*.el")))
(package! cape)
(package! kind-icon)
;; (package! citre)
