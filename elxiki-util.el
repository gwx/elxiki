;;; elxiki-util.el --- Various utility commands.

(require 'pprint)

(defun elxiki-wrap-text (string &optional width)
  "Wraps STRING around target WIDTH."
  (let ((string (pprint-to-string string width)))
    (substring string 1 (1- (length string)))))

(defun elxiki-prefix-buffer (string)
  "Add STRING as a prefix to every line in the buffer."
  (save-excursion
    (goto-char (point-min))
    (insert string)
    (while (elxiki/forward-line)
      (insert string))))

(defun elxiki-prefix-p (prefix string)
  "Return non-nil if PREFIX is a prefix of STRING."
  (and (>= (length string) (length prefix))
       (string-equal prefix
                     (substring string 0 (length prefix)))))

(defun elxiki-drop-prefix (prefix string)
  "Remove PREFIX from STRING. Return nil if unable to do so."
  (when (elxiki-prefix-p prefix string)
    (substring string (length prefix))))

(defun elxiki/ends-slash-p (string)
  "If STRING ends with a /, followed by whitespace."
  (save-match-data
    (string-match (rx "/" (* blank) string-end) string)))

(defun elxiki/strip-slash (string)
  "Strips the ending slash from STRING."
  (save-match-data
    (if (string-match (rx "/" string-end) string)
        (substring string 0 (1- (length string)))
      string)))

(defun elxiki/path-root (path)
  "Return the first part of PATH."
  (save-match-data
    (when (string-match (rx string-start (* blank)
                            (group (+ (not (any "/")))))
                        path)
      (match-string 1 path))))

(defun elxiki-drop-root (path)
  "Remove the first element of PATH."
  (replace-regexp-in-string
   (rx string-start (* (not (any "/"))) (? "/"))
   ""
   path))

(defun elxiki/name-equal (name1 name2)
  "Return non-nil if NAME1 and NAME2 are equal, disregarding a final /."
  (string-equal (elxiki/strip-slash name1)
                (elxiki/strip-slash name2)))

(defun elxiki/normalize-buffer-indentation ()
  "Remove excess indentation from buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((indent (current-indentation)))
      (indent-line-to 0)
      (while (elxiki/forward-line)
        (indent-line-to (max 0 (- (current-indentation) indent)))))))

(defun elxiki/normalize-indentation (strings)
  "Left justify the list of STRINGS."
  (let ((indent (with-temp-buffer
                  (insert (car strings))
                  (goto-char (point-min))
                  (current-indentation))))
    (mapcar (lambda (string) 
              (with-temp-buffer
                (insert string)
                (indent-line-to (max 0 (- (current-indentation) indent)))
                (buffer-string)))
            strings)))

(defun elxiki/line-blank (&optional pos)
  "If the line at POS is blank.
POS defaults to point."
  (save-excursion
    (when pos (goto-char pos))
    (forward-line 0)
    (looking-at-p (rx line-start (* blank) line-end))))

(defun elxiki/indentaion ()
  "Get the current indentation."
  (if (elxiki/line-blank)
      most-positive-fixnum
    (current-indentation)))

(defun elxiki/forward-line ()
  "Move forward 1 line. Return non-nil if it succeeds."
  (forward-line 0)
  (let ((pos (point)))
    (forward-line 1)
    (forward-line 0)
    (not (= pos (point)))))

(provide 'elxiki-util)

;;; elxiki-util.el ends here
