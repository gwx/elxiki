;;; elxiki-line.el --- Manipulate elxiki lines.

;;; Commentary:
;; Functions to retrieve information on and manipulate the elxiki line
;; at point.

;;; Code:
(defvar elxiki-line-prefix-list
  '("| " "$ " "% " "+ " "- " "* " "! ")
  "List of prefixes which are used by elxiki.")

(defun elxiki/region (start length)
  "Return the valid region starting from START up to LENGTH long."
  (list start
        (min (point-max)
             (+ length start))))

(defun elxiki/match-buffer (string &optional pos)
  "Match STRING against the characters following POS.
POS defaults to point."
  (unless pos (setq pos (point)))
  (string-equal string
                (buffer-substring-no-properties
                 pos
                 (min (point-max) (+ pos (length string))))))

(defun elxiki/trim (string)
  "Remove whitespace from edges of STRING."
  (save-match-data
    (string-match (rx string-start (* blank)
                      (group (*? anything))
                      (* blank) string-end)
                  string)
    (match-string 1 string)))

(defun elxiki-line-get-prefix (&optional pos)
  "Gets the prefix of the elxiki line at POS.
POS defaults to point.  Returns nil if there is no prefix or valid
line."
  (save-excursion
    (when pos (goto-char pos))
    (forward-to-indentation 0)
    (let ((prefixes elxiki-line-prefix-list))
      (while (and prefixes
                  (not (elxiki/match-buffer (car prefixes))))
        (setq prefixes (cdr prefixes)))
      (car prefixes))))

(defun elxiki-line-get (&optional pos)
  "Gets the elxiki line at POS, splitting it into (prefix name).
POS defaults to point.  Returns nil if the line at POS is not a
valid elxiki line."
  (save-excursion
    (save-match-data
      (when pos (goto-char pos))
      (let ((prefix (elxiki-line-get-prefix))
            name start)
        ;; Find name
        (forward-to-indentation 0)
        (forward-char (length prefix))
        (setq start (point))
        (end-of-line)
        (setq name (elxiki/trim (buffer-substring-no-properties start (point))))
        ;; Check for eligibility
        (when (or prefix
                  (string-match (rx "/" (* blank) string-end) name))
          (list prefix name))))))

(defun elxiki-line-get-name (&optional pos)
  "Gets the name of the elxiki line at POS.
POS defaults to point.  Returns nil if there is no valid elxiki
line."
  (nth 1 (elxiki-line-get pos)))

(defun elxiki-line-find-parent (&optional pos)
  "Return the position of the parent of elxiki line at POS.
If POS is not specified, defaults to point.  Returns nil if
parent does not exist, or POS is not at an exiki line."
  (save-excursion
    (when pos (goto-char pos))
    (setq pos (point))
    (let ((indent (current-indentation))
          (line (line-number-at-pos)))
      (while (and (elxiki-line-get)
                  (>= (current-indentation) indent)
                  (= 0 (forward-line -1))))
      (when (and (elxiki-line-get)
                 (not (= (line-number-at-pos) line)))
        (forward-to-indentation 0)
        (point)))))

(defun elxiki-line-find-child (&optional pos)
  "Return the position of the first child of elxiki line at POS.
If POS is not specified, defaults to point.  Returns nil if the
child does not exist, or POS is not at an elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (let ((indent (current-indentation)))
      (when (and (elxiki-line-get)
                 (forward-to-indentation 1)
                 (elxiki-line-get)
                 (> (current-indentation) indent))
        (point)))))

(defun elxiki-line-find-sibling (&optional pos)
  "Return the position of the first sibling after elxiki line at POS.
If POS is not specified, defaults to point.  Returns nil if the
sibling does not exist, or POS is not at an elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (let ((indent (current-indentation)))
      (while (and (elxiki-line-get)
                  (forward-to-indentation 1)
                  (> (current-indentation) indent)))
      (when (and (elxiki-line-get)
                 (= (current-indentation) indent))
        (point)))))

(defun elxiki-line-do-all-children (function &optional pos)
  "Run FUNCTION once with point set for each child of line at POS.
POS defaults to point."
  (save-excursion
    (when pos (goto-char pos))
    (goto-char (elxiki-line-find-child))
    (funcall function)
    (while (setq pos (elxiki-line-find-sibling))
      (goto-char pos)
      (funcall function))))

(defun elxiki-line-find-all-children (&optional pos)
  "Return the (start end) region of all children of elxiki line at POS.
If POS is not specified, defaults to point.  Returns nil if there
are no children, or POS is not at an elxiki line."
  (when (elxiki-line-find-child pos)
    (save-excursion
      (when pos (goto-char pos))
      (forward-line 1)
      (let ((start (point))
            (indent (current-indentation)))
        (while (and (elxiki-line-get)
                    (>= (current-indentation) indent)
                    (= 0 (forward-line 1))))
        (if (and (elxiki-line-get)
                 (>= (current-indentation) indent))
            (end-of-line)
          (forward-line 0))
        (list start (point))))))

(defun elxiki-line-find-self (&optional pos)
  "Return the (start end) region of line at POS and all its children.
If POS is not specified, it defaults to point. Return nil if this
is not a valid elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (when (elxiki-line-get)
      (let ((children (elxiki-line-find-all-children))
            start end)
        (forward-line 0)
        (setq start (point))
        (if children
            (setq end (nth 1 children))
          (end-of-line)
          (setq end (point)))
        (list start end)))))

(defun elxiki-line-add-children (children &optional prefix-function pos)
  "Add CHILDREN underneath of the elxiki line at POS, properly indented.
CHILDREN can either be a string or a list of strings. If it is a
string, it is converted into a list of strings by splitting on
the newline character.  If PREFIX-FUNCTION is specified, then
apply it to every line before using it. If POS is not specified,
it defaults to point.  Returns nil if POS is not an elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (when (stringp children)
      (setq children (split-string children "\n" 'no-blanks)))
    (when (elxiki-line-get)
      (let ((blank-prefix (apply 'concat
                           (make-list (+ 2 (current-indentation)) " "))))
        (end-of-line)
        (dolist (child children)
          (when (functionp prefix-function)
            (setq child (funcall prefix-function child)))
          (insert "\n")
          (insert blank-prefix)
          (insert child))))))

(defun elxiki-line-set-prefix (prefix &optional pos)
  "Changes elxiki line at POS to have PREFIX.
POS defaults to point."
  (save-excursion
    (when pos (goto-char pos))
    (forward-to-indentation 0)
    (let ((old-prefix (elxiki-line-get-prefix)))
      (apply 'delete-region (elxiki/region (point) (length old-prefix)))
      (insert prefix))))

(defun elxiki-line-get-ancestry (&optional pos)
  "Gets the ancestry of elxiki line at POS.
POS defaults to point."
  (save-excursion
    (when pos (goto-char pos))
    (let (ancestry line)
      (while (and (setq line (elxiki-line-get))
                  (setq ancestry (cons line ancestry))
                  (setq pos (elxiki-line-find-parent)))
        (goto-char pos))
      ancestry)))

(provide 'elxiki-line)
;;; elxiki-line.el ends here
