((python-mode
  . (
     ;; This is used to fill the paragraph in 120 columns when pressing M-q
     ;; because in the Mozio project we use 120 columns
     (fill-column . 120)

     ;; lsp-ui is already disabled
     ;; Only enable python-pycheckers
     (eval . (setq flycheck-checker 'python-pycheckers))

     (pyvenv-workon . "fabriclb-nana-371")

     )))
