;;; elxiki-util.el --- Various utility commands.

(defun elxiki-change-line (&optional n)
  "Move to the beginning of the line, relative N lines from this one.
Return point if succesful, or nil if unsuccessful. If
unsuccessful, do not move point. The default value for N is 0."
  (unless n (setq n 0))
  (let ((origin (point))
        start end)
    (forward-line 0)
    (setq start (point))
    (forward-line n)
    (forward-line 0)
    (setq end (point))
    (cond ((= n 0) (point))
          ((= (abs n) (count-lines start end))
           end)
          ('else
           (goto-char origin)
           nil))))

(put 'elxiki-negative-indent-error
     'error-conditions
     '(error elxiki-error elxiki-negative-indent-error))
(put 'elxiki-negative-indent-error
     'error-message
     "Attempted negative indent")

(defun elxiki-change-indentation (n &optional no-error)
  "Adjust indentation of line at point by N.
If this would result in negative indentation, instead set it to
0, and signal an error if NO-ERROR is nil. Return t if
successful."
  (save-excursion
    (forward-to-indentation 0)
    (while (and (< n 0)
                (not (bolp)))
      (delete-char -1)
      (setq n (1+ n)))
    (while (> n 0)
      (insert " ")
      (setq n (1- n)))
    (if (and (not no-error) (< n 0))
        (signal 'elxiki-negative-indent-error nil)
      t)))

(defun elxiki-normalize-indentation (start end &optional indent no-error)
   "From line at START to line at END, normalize to INDENT indentation.
Relative indentation of the lines is kept. If NO-ERROR is
non-nil, ignore errors resulting from attempting to create
negative indentation.  INDENT defaults to 0. Does not move point."
   (save-excursion
     (goto-char start)
     (forward-line 0)
     (setq start (point))
     (setq indent (- (or indent 0) (current-indentation)))
     (goto-char end)
     (while (and (>= (point) start)
                 (not (bobp)))
       (elxiki-change-indentation indent no-error)
       (forward-line -1))
     (when (and (bobp) (= (point) start))
       (elxiki-change-indentation indent no-error))))

(defun elxiki-strip-end-fslash (string)
  "Strips the ending forward slash from STRING."
  (when string
    (replace-regexp-in-string (rx "/" (* blank) string-end)
                              ""
                              string)))

(defun elxiki-name-equal (name1 name2)
  "Return non-nil if NAME1 and NAME2 are equal, disregarding a final /."
  (string-equal (elxiki-strip-end-fslash name1)
                (elxiki-strip-end-fslash name2)))

(defun elxiki-narrow-to-lines (start end)
  "Narrow to region containing lines."
  (goto-char start)
  (forward-line 0)
  (setq start (point))
  (goto-char end)
  (end-of-line)
  (narrow-to-region start (point)))

(defun elxiki-doto-lines (function start end)
  "Perform FUNCTION at the start of every line from START to END.
Uses the lines that START and END are on, even if these points
are halfway through these lines. FUNCTION should not change the
line."
  (save-excursion
    (save-restriction
      (elxiki-narrow-to-lines start end)
      (goto-char (point-min))
      (funcall function)
      (while (elxiki-change-line 1)
        (funcall function)))))

(defun elxiki-path-root (path)
  "Drop PATH after first / character."
  (when path
    (save-match-data
      (when (string-match (rx string-start (* blank)
                              (group (* (not (any "/"))) (? "/")))
                          path)
        (match-string 1 path)))))










(defmacro elxiki-get-point (&rest forms)
  "Evaluate FORMS, return (point), and then reset point."
  `(save-excursion ,@forms (point)))

(defun elxiki/region (start length)
  "Return the valid region starting from START up to LENGTH long."
  (list start
        (min (point-max)
             (+ length start))))

(defun elxiki/trim (string)
  "Remove whitespace from edges of STRING."
  (save-match-data
    (string-match (rx string-start (* blank)
                      (group (*? anything))
                      (* blank) string-end)
                  string)
    (match-string 1 string)))

(defun elxiki/match-buffer (string &optional pos)
  "Match STRING against the characters following POS.
POS defaults to point."
  (unless pos (setq pos (point)))
  (string-equal string
                (buffer-substring-no-properties
                 pos
                 (min (point-max) (+ pos (length string))))))

(defun elxiki-wrap-text (string &optional width)
  "Wraps STRING around target WIDTH."
  (let ((string (format "%S" string width)))
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
