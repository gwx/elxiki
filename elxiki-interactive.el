;;; elxiki-interactive.el --- Extra interactive commands for use with elxiki.

;;; Code:
(defun elxiki-copy-name (&optional pos)
  "Copies the name of the elxiki line at point to the kill ring."
  (interactive)
  (kill-new (elxiki-line-get-name pos)))

(defun elxiki-kill-branch (&optional pos)
  "Kills the elxiki line at point and all its children."
  (interactive)
  (save-excursion
    (when pos (goto-char pos))
    (let (start end)
      (beginning-of-line)
      (setq start (point))
      (setq end (nth 1 (elxiki-line-find-all-children)))
      (kill-region start end))))

(provide 'elxiki-interactive)
;;; elxiki-interactive.el ends here
