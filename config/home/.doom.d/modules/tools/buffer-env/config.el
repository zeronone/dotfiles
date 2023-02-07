;;; tools/buffer-env/config.el -*- lexical-binding: t; -*-

(use-package! buffer-env
  :demand t
  :config
  (add-hook! 'hack-local-variables-hook #'buffer-env-update))
