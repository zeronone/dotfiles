;;; lang/java/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :hook (java-mode . lsp-mode))

(def-package! lsp-ui
  :hook (lsp-after-open . lsp-ui-mode))

(def-package! company-lsp
  :config
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-cache-candidates t)
  (set-company-backend! 'lsp-mode 'company-lsp))

(def-package! lsp-intellij
  :hook (java-mode . lsp-intellij-enable))

