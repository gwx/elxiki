;;; elxiki-util.el --- Various utility commands.

(defun elxiki/ends-slash-p (string)
  "If STRING ends with a /, followed by whitespace."
  (save-match-data
    (string-match (rx "/" (* blank) string-end) string)))

(defun elxiki/path-root (path)
  "Returns the first part of PATH"
  (save-match-data
    (when (string-match (rx string-start (* blank)
                            (group (+ (not "/")))
                            "/")
                        path)
      (match-string 1 path))))

(provide 'elxiki-util)

;;; elxiki-util.el ends here
