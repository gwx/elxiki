;;; elxiki-line.el --- Manipulate elxiki lines.

;;; Commentary:
;; Functions to retrieve information on and manipulate the elxiki line
;; at point.

;;; Code:
(defvar elxiki-line-prefix-list
  '("| " "$ " "% " "+ " "- " "& " "! " "> " "@ " "!! " "|")
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
                  (string-match (rx "/" (* blank) string-end) name)
                  (string-match (rx string-start (not blank)) name)
                  (string-match (rx string-start (* blank) string-end) name))
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
                  (or (>= (current-indentation) indent)
                      (elxiki/line-blank))
                  (= 0 (forward-line -1))))
      (when (and (elxiki-line-get)
                 (< (current-indentation) indent)
                 (not (= (line-number-at-pos) line)))
        (forward-to-indentation 0)
        (point)))))

(defun elxiki-line-goto-parent (&optional pos)
  "Goto the result of `elxiki-line-find-parent'."
  (let ((n (elxiki-line-find-parent pos)))
    (when n
      (goto-char n))))

(defun elxiki-line-goto-next ()
  "Goto the elxiki line following point if it exists.
Returns point if the line is found and nil if it is not. Non
elxiki lines will interrupt this."
  (let ((posn (point)))
    (while (and (elxiki/forward-line)
                (elxiki/line-blank)))
    (forward-line 0)
    (if (= (point) posn)
        nil
      (point))))

(defun elxiki-line-goto-current ()
  "Goto the elxiki line before or at point if it exists.
Returns point if the line is found and nil if it is not. Non
elxiki lines will interrupt this."
  (let ((posn (point)))
    (while (and (elxiki/line-blank)
                (= 0 (forward-line -1))))
    (forward-line 0)
    (when (elxiki-line-get)
      (point))))

(defun elxiki-line-goto-name ()
  "Goto the name portion of the current elxiki item."
  (elxiki-line-goto-current)
  (let ((line (elxiki-line-get)))
    (when line
      (forward-to-indentation 0)
      (forward-char (length (car line)))
      (point))))

(defun elxiki-line-find-first-child (&optional pos)
  "Return the position of the first child of elxiki line at POS.
If POS is not specified, defaults to point.  Returns nil if the
child does not exist, or POS is not at an elxiki line."
  (when pos (goto-char pos))
  (let ((indent (current-indentation)))
    (save-excursion
      (when (and (elxiki-line-get)
                 (elxiki-line-goto-next)
                 (> (current-indentation) indent))
        (point)))))

(defun elxiki-line-goto-first-child (&optional pos)
  "Goto the result of `elxiki-line-find-first-child'."
  (let ((n (elxiki-line-find-first-child pos)))
    (when n
      (goto-char n))))

(defun elxiki-line-find-first-sibling (&optional pos)
  "Return the position of the first sibling after elxiki line at POS.
If POS is not specified, defaults to point.  Returns nil if the
sibling does not exist, or POS is not at an elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (forward-line 0)
    (let ((indent (current-indentation))
          (start (point)))
      (while (and (elxiki-line-goto-next)
                  (> (current-indentation) indent)))
      (and (elxiki-line-get)
           (= (current-indentation) indent)
           (not (= start (point)))
           (point)))))

(defun elxiki-line-goto-first-sibling (&optional pos)
  "Goto the result of `elxiki-line-find-first-sibling'."
  (let ((n (elxiki-line-find-first-sibling pos)))
    (when n
      (goto-char n))))

(defun elxiki-line-find-append (&optional pos)
  "Return the position where a sibling would be appended to at POS.
If POS is not specified, defaults to point.  Returns nil if the
POS is not at an elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (forward-line 0)
    (let ((indent (current-indentation))
          (start (point)))
      (while (and (elxiki/forward-line)
                  (or (elxiki/line-blank)
                      (>= (current-indentation) indent))))
      (if (< (current-indentation) indent)
          (progn (forward-line 0)
                 (point))
        (unless (elxiki/forward-line)
          (end-of-line)))
      (when (not (= start (point)))
        (point)))))

(defun elxiki-line-goto-append (&optional pos)
  "Goto the result of `elxiki-line-find-append'."
  (let ((n (elxiki-line-find-append pos)))
    (when n
      (goto-char n))))

(defun elxiki-line-append-sibling (line &optional pos)
  "Append LINE as a sibling of the elxiki line at POS.
POS defaults to point. The added line has its indentation
matched. Return the line's position."
  (when (elxiki-line-get)
    (save-excursion
      (when pos (goto-char pos))
      (let ((indent (current-indentation)))
        (elxiki-line-goto-append)
        (insert "\n")
        (forward-line -1)
        (indent-line-to indent)
        (insert line)
        (forward-to-indentation 0)
        (point)))))

(defun elxiki-line-append-child (line &optional pos)
  "Append LINE as a child of the elxiki line at POS.
POS defaults to point. Return the line's position."
  (when (elxiki-line-get)
    (save-excursion
      (when pos (goto-char pos))
      (if (elxiki-line-goto-first-child)
          (elxiki-line-append-sibling line)
        (let ((indent (current-indentation)))
          (forward-line 1)
          (insert "\n")
          (forward-line -1)
          (indent-line-to (+ 2 indent))
          (insert line)
          (forward-to-indentation 0)
          (point))))))

(defun elxiki-line-find-sibling (name &optional create pos)
  "Return the position of sibling named NAME at or after POS.
POS defaults to point. May return the current line. If CREATE is
non-nil, then create the sibling if it does not exist."
  (when (elxiki-line-get)
    (save-excursion
      (when pos (goto-char pos))
      (let ((start (point)))
        (while (and (not (elxiki/name-equal name (elxiki-line-get-name)))
                    (elxiki-line-goto-first-sibling)))
        (if (elxiki/name-equal name (elxiki-line-get-name))
            (point)
          (elxiki-line-append-sibling 
           (concat "- " name)))))))

(defun elxiki-line-goto-sibling (name &optional create pos)
  "Goto the result of `elxiki-line-find-sibling'."
  (let ((n (elxiki-line-find-sibling name create pos)))
    (when n
      (goto-char n))))

(defun elxiki-line-find-child (name &optional create pos)
  "Return the position of child named NAME.
POS defaults to point. If CREATE is non-nil, then create the
child if it does not exist."
  (when (elxiki-line-get)
    (save-excursion
      (when pos (goto-char pos))
      (if (elxiki-line-goto-first-child)
          (elxiki-line-find-sibling name create)
        (when create
          (elxiki-line-append-child (concat "- " name)))))))

(defun elxiki-line-goto-child (name &optional create pos)
  "Goto the result of `elxiki-line-find-child'."
  (let ((n (elxiki-line-find-child name create pos)))
    (when n
      (goto-char n))))

(defun elxiki-line-find-route (route &optional create pos)
  "Return the elxiki line position that results from following ROUTE.
If POS is defined, start from there, otherwise start from point.
ROUTE is a list of strings. Finds each child named by the each
element of ROUTE in turn.  If ROUTE is a string, then split it
along the '/' character.

If CREATE is non-nil, then create the route if it does not
exist."
  (when (stringp route)
    (setq route (split-string route "/" 'strip-empty)))
  (when (elxiki-line-get)
    (save-excursion
      (when pos (goto-char pos))
      (catch 'route
        (while route
          (unless (elxiki-line-goto-sibling (car route) create)
            (throw 'route nil))
          (when (setq route (cdr route))
            (unless (elxiki-line-goto-first-child)
              (if create
                  (elxiki-line-goto-child (car route) 'create)
                (throw 'route nil)))))
        (forward-to-indentation 0)
        (point)))))

(defun elxiki-line-goto-route (route &optional create pos)
  "Goto the result of `elxiki-line-find-route'."
  (let ((n (elxiki-line-find-route route create pos)))
    (when n
      (goto-char n))))

;; (defun elxiki-line-do-all-children (function &optional pos)
;;   "Run FUNCTION once with point set for each child of line at POS.
;; POS defaults to point."
;;   (save-excursion
;;     (when pos (goto-char pos))
;;     (goto-char (elxiki-line-find-child))
;;     (funcall function)
;;     (while (setq pos (elxiki-line-find-sibling))
;;       (goto-char pos)
;;       (funcall function))))

(defun elxiki-line-find-all-children (&optional pos)
  "Return the (start end) region of all children of elxiki line at POS.
If POS is not specified, defaults to point.  Returns nil if there
are no children, or POS is not at an elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (forward-line 0)
    (when (elxiki-line-goto-first-child)
      (let ((start (point)))
        (elxiki-line-goto-append)
        (list start (point))))))

(defun elxiki-line-find-self (&optional pos)
  "Return the (start end) region of line at POS and all its children.
If POS is not specified, it defaults to point. Return nil if this
is not a valid elxiki line."
  (save-excursion
    (when pos (goto-char pos))
    (when (elxiki-line-get)
      (forward-line 0)
      (let ((start point))
        (or (elxiki-line-goto-first-sibling)
            (elxiki-line-goto-append))
        (list start (point))))))

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
  ;; `save-excursion' doesn't work if we're inside the prefix already,
  ;; so save point manually.
  (let ((old-pos (point)))
    (when pos (goto-char pos))
    (forward-to-indentation 0)
    (let ((old-prefix (elxiki-line-get-prefix)))
      (apply 'delete-region (elxiki/region (point) (length old-prefix)))
      (insert prefix))
    (goto-char old-pos)))

(defun elxiki-line-fold (&optional pos)
  "Remove all children from the elxiki line at POS.
If the prefix is currently \"- \", change it to \"+ \".  POS
defaults to point."
  (save-excursion
    (when pos (goto-char pos))
    (let ((children (elxiki-line-find-all-children)))
      (when (string-equal "- " (elxiki-line-get-prefix))
        (elxiki-line-set-prefix "+ "))
      (when children
        (apply 'delete-region children)))))

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

(defun elxiki-line-match-siblings (predicate)
  "Return a list of offsets for siblings which match PREDICATE.
PREDICATE is a function of no arguments which is run when point
is at the start of the sibling."
  (save-excursion
    (forward-line 0)
    (let ((matches (when (funcall predicate) (list 0)))
          (offset 0))
      (while (elxiki-line-goto-first-sibling)
        (setq offset (1+ offset))
        (when (funcall predicate)
          (setq matches (cons offset matches))))
      (nreverse matches))))

(defun elxiki-line-delete-siblings ()
  "Delete self and all siblings after."
  (save-excursion
    (let ((start (point)))
      (when (elxiki-line-goto-append)
        (delete-region start (point))))))

(defun elxiki-line-delete-branch ()
  "Deletes the elxiki line and children at point."
  (save-excursion
    (forward-line 0)
    (let ((start (point)))
      (when (or (elxiki-line-goto-first-sibling)
                (elxiki-line-goto-append))
        (delete-region start (point))))))

(defun elxiki-line-filter-siblings (predicate &optional restrict-none)
  "Remove all siblings who do not satisfy PREDICATE.
PREDICATE is run when point is at the start of the sibling. if
RESTRICT-NONE is non-nil, then do not filter if it would result
in no siblings left."
  (save-excursion
    (elxiki-line-goto-current)
    (let ((matches (elxiki-line-match-siblings predicate))
          (indent (current-indentation))
          (offset 0))
      (cond
       (matches
        (while matches
          (while (< offset (car matches))
            (elxiki-line-delete-branch)
            (setq offset (1+ offset)))
          (setq offset (1+ offset))
          (if (elxiki-line-goto-first-sibling)
              (setq matches (cdr matches))
            (setq matches nil)
            (elxiki-line-goto-append)))
        (when (and (elxiki-line-goto-current)
                   (>= (current-indentation) indent))
          (elxiki-line-delete-siblings)))
       ((not restrict-none)
        (elxiki-line-delete-siblings))))))

(defun elxiki-line-filter-children (predicate &optional restrict-none)
  "Remove all children who do not satisfy PREDICATE.
PREDICATE is run when point is at the start of the child. if
RESTRICT-NONE is non-nil, then do not filter if it would result
in no siblings left."
  (save-excursion
    (when (elxiki-line-goto-first-child)
      (elxiki-line-filter-siblings predicate restrict-none))))

(provide 'elxiki-line)
;;; elxiki-line.el ends here
