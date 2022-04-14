;;; indeed.el -*- lexical-binding: t; -*-

(after! vterm
  ;; Set the default login shell for each tramp method
  (setq vterm-tramp-shells
        '(("docker" "/bin/sh")
          ("ssh" "/bin/bash -il"))))



;; executes after the `after! tramp` in core-editor.el
(after! tramp
  ;; For debugging
  ;; (setq tramp-verbose 10)
  ;; (setq tramp-debug-to-file t)

  (setq tramp-histfile-override nil)

  ;; Default used if there is value set
  (setq tramp-default-remote-shell "/bin/bash")

  ;; This program is used for encoding/decoding in localhost
  ;; Default is /bin/sh which don't expands ~
  (setq tramp-encoding-shell "/bin/zsh")

  ;; tramp-sh-extra-args is used in two places
  ;;   when setting up remote shells
  ;;   when setting up local encoding shells
  (setq tramp-sh-extra-args
        '(
          ("/bash\\'" . "-noediting -norc -noprofile")
          ("/zsh\\'" . "-f +Z -V")
          ))

  ;; Avoids the update-manage-repos-check at login
  (add-to-list 'tramp-remote-process-environment "INDEED_ENV_IN_DOCKER=1")

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "arezai.cvm.indeed.net")
                     "remote-shell-login" '("-il")))

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "arezai.cvm.indeed.net")
                     "remote-shell" "/bin/bash"))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(after! lsp-mode
  (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-tramp-connection '("typescript-language-server" "--stdio"))
                      :major-modes '(js-mode rjsx-mode typescript-mode typescript-tsx-mode)
                      :priority 2
                      :remote? t
                      :server-id 'ts-ls-remote-indeed)))
