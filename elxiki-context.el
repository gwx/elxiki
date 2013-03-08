;;; elxiki-context.el --- How to interpret a given elxiki line.

;;; Commentary:
;; This is where elxiki interprets information about the elxiki
;; lines. To extend, take a look at `elxiki-context-functions' and
;; `elxiki-context-post-functions'.

;;; Code:
(defun elxiki-context-from-ancestry (ancestry)
  "Takes a list of line infos and returns them as a context alist.
ANCESTRY should be of the form returned by `elxiki-line-get-ancestry'."
  (save-match-data
    (let ((path "")
          prefix name)
      (dolist (ancestor ancestry)
        (setq prefix (nth 0 ancestor))
        (setq name (nth 1 ancestor))
        ;; Match things that end with /.
        (when (string-match (rx "/" (* blank) string-end) name)
          (setq path (concat path name))))
      `((prefix . ,prefix)
        (name . ,name)
        (path . ,path)))))

(defun elxiki-context-get-prefix (context)
  "Retrieve the prefix from CONTEXT."
  (cdr (assoc 'prefix context)))

(defun elxiki-context-get-name (context)
  "Retrieve the name from CONTEXT."
  (cdr (assoc 'name context)))

(defun elxiki-context-get-path (context)
  "Retrieve the path from CONTEXT."
  (cdr (assoc 'path context)))

(defun elxiki-context-default-directory (context)
  "Gets the default directory from CONTEXT."
  (let ((path (cdr (assoc 'path context))))
    (if (and path (member (string-to-char path) '(?. ?~ ?/)))
        (expand-file-name path)
      (expand-file-name default-directory))))

(defun elxiki-context-directory-p (context)
  "If CONTEXT describes a directory line (that exists)."
  (let ((default-directory (elxiki-context-default-directory context))
        (name (elxiki-context-get-name context)))
    (and default-directory
         (elxiki/ends-slash-p name)
         (file-directory-p (expand-file-name name)))))

(provide 'elxiki-context)
;;; elxiki-context.el ends here
