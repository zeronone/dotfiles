;;; scripts.el -*- lexical-binding: t; -*-


;;;
;;; Remove :drill: from the first heading of all the files in the specified directory

(defun remove-org-mode-tag (tag-to-remove)
  "Remove :drill: tag."
  (let (tags)
    (save-excursion
      (org-back-to-heading t)
      (setq tags (org-element-property :tags (org-element-at-point))))
    (when (and tags (member tag-to-remove tags))
      (setq tags (delete tag-to-remove tags))
      (org-set-tags-to tags))))

(--each (directory-files-recursively "~/Dropbox/orgs/mywiki/algorithmic-problems" "")
  (with-temp-buffer

    (insert-file-contents it)
    (org-mode)
    (if (re-search-forward "\*" nil t)
        (progn

          (setq tags (org-element-property :tags (org-element-at-point)))
          (message "file: %s, tags: %s" it tags)
          (remove-org-mode-tag "drill")

          (write-region nil nil it nil nil)))))


;;;
;;;
;;;

(defun subtree-to-new-file ()
  "Sloppily assists in moving an org subtree to a new file, if base-directory is defined it will be placed there"
  (interactive)
  (setq header-text (org-entry-get nil "ITEM"))
  (org-copy-subtree nil t)
  (setq new-filename (concat (s-snake-case header-text) ".org"))
  (find-file-other-window (expand-file-name new-filename (or base-directory org-directory)))
  (erase-buffer)
  (insert (concat "#+TITLE: " header-text "\n"))
  (insert "\n")
  (org-paste-subtree)
  (delete-window))
