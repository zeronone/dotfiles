;;; ob-minizinc.el -*- lexical-binding: t; -*-

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-tangle-lang-exts) ;; Autoloaded
(add-to-list 'org-babel-tangle-lang-exts '("minizinc" . "mzn"))

(defvar org-babel-default-header-args:minizinc
  '((:results . "output") (:exports . "results") (:cmd . "minizinc") (:fileext . ".mzn"))
  "Default arguments to use when evaluating a dot source block.")

(defun org-babel-execute:minizinc (body params)
  (let* ((cmdline (or (cdr (assoc :cmdline params)) ""))
         (full-body (org-babel-expand-body:generic body params))
         (cmd (or (cdr (assoc :cmd params)) "minizinc"))
         (fileext (or (cdr (assoc :fileext params)) ".mzn"))
         (in-file (org-babel-temp-file "mzn-" fileext)))

    (with-temp-file in-file (insert full-body))
    (org-babel-eval
     (concat cmd
             " " cmdline
             " " (org-babel-process-file-name in-file))
     "")))

(provide 'ob-minizinc)
;;; ob-java.el ends here
