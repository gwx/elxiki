;;; elxiki-util.el --- Various utility commands.

(defun elxiki/ends-slash-p (string)
  "If STRING ends with a /, followed by whitespace."
  (save-match-data
    (string-match (rx "/" (* blank) string-end) string)))

(provide 'elxiki-util)

;;; elxiki-util.el ends here
