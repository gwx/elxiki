;;; elxiki-line.el --- Manipulate elxiki lines.

;;; Commentary:
;; Functions to retrieve information on and manipulate the elxiki line
;; at point.

;;; Code:
(defvar elxiki-line-prefix-list
  '("| " "|" "$ " "% " "+ " "- " "& " "! " "> " "@ " "!! ")
  "List of prefixes which are used by elxiki.")

(defvar elxiki-line-indent-count 2
  "How many spaces to indent children.")

(defun elxiki-line-get-indentation ()
  "Return the amount of indentation of line at point."
  (current-indentation))

(defun elxiki-line-indent-to (indent)
  "Indents the current line to INDENT spaces."
  (indent-line-to indent))

(defun elxiki-line-goto-beginning ()
  "Move point to the beginning of the current elxiki line."
  (forward-line 0))

(defun elxiki-line-blank-p ()
  "If the line at point is blank."
  (save-excursion
    (elxiki-line-goto-beginning)
    (looking-at-p (rx (* blank) line-end))))

(defun elxiki-line-goto-prefix ()
  "Move point to the prefix of the current elxiki line."
  (forward-to-indentation 0))

(defun elxiki-line-looking-at-prefix ()
  "If there is a prefix at point, return it."
    (let ((prefixes elxiki-line-prefix-list))
      (while (and prefixes
                  (not (looking-at-p (regexp-quote (car prefixes)))))
        (setq prefixes (cdr prefixes)))
      (car prefixes)))

(defun elxiki-line-goto-name ()
  "Goto the name portion of the elxiki line at point."
  (elxiki-line-goto-prefix)
  (let ((prefix (elxiki-line-looking-at-prefix)))
    (when prefix
      (forward-char (length prefix)))))

(defun elxiki-line-get-prefix ()
  "Gets the prefix of the elxiki line at point.
Returns nil if there is no prefix or valid line."
  (save-excursion
    (elxiki-line-goto-prefix)
    (elxiki-line-looking-at-prefix)))

(defun elxiki-line-get-name ()
  "Gets the name of the elxiki line at point.
Returns nil if there is no valid elxiki line."
  (save-excursion
    (elxiki-line-goto-name)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring start (point)))))

(defun elxiki-line-valid-p ()
  "Return non-nil if there is an elxiki line at point."
  (or (elxiki-line-get-prefix)
      (save-excursion (elxiki-line-goto-name)
        (or (looking-at-p (rx (* nonl) "/" (* blank) line-end))
            (looking-at-p (rx line-start (* blank) line-end))))))

(defun elxiki-line-goto-current ()
  "Goto the elxiki line before or at point if it exists.
Returns point if the line is found and nil if it is not. Non
elxiki lines will interrupt this."
  (let ((posn (point)))
    (elxiki-line-goto-beginning)
    (while (and (elxiki-line-blank-p)
                (elxiki-change-line -1)))
    (if (elxiki-line-valid-p)
        (point)
      (goto-char posn)
      nil)))

(defun elxiki-line-goto-next ()
  "Goto the elxiki line following point if it exists.
Returns point if the line is found and nil if it is not. Non
elxiki lines will interrupt this."
  (when (elxiki-line-valid-p)
    (let ((posn (point)))
      (while (and (elxiki-change-line 1)
                  (elxiki-line-blank-p)))
      (if (and (not (= (point) posn))
               (elxiki-line-valid-p))
          (point)
        (goto-char posn)
        nil))))

(defun elxiki-line-goto-previous ()
  "Goto the elxiki line preceding point if it exists.
Returns point if the line is found and nil if it is not. Non
elxiki lines will interrupt this."
  (let ((start (point))
        (current (elxiki-line-goto-current)))
    (when current
      (while (and (elxiki-change-line -1)
                  (elxiki-line-blank-p)))
      (if (and (not (= (point) current))
               (elxiki-line-valid-p))
          (point)
        (goto-char start)
        nil))))

(defun elxiki-line-goto-parent ()
  "Goto the parent of the elxiki line at point.
Return point if successful. If unsuccessful, return nil and don't
move point."
  (let ((start (point))
        (indent (elxiki-line-get-indentation)))
    (when (and (> indent 0)
               (elxiki-line-valid-p))
      (while (and (or (elxiki-line-valid-p)
                      (elxiki-line-blank-p))
                  (or (>= (elxiki-line-get-indentation) indent)
                      (elxiki-line-blank-p))
                  (elxiki-change-line -1)))
      (if (and (elxiki-line-valid-p)
               (< (elxiki-line-get-indentation) indent))
          (point)
        (goto-char start)
        nil))))

(defun elxiki-line-goto-first-child ()
  "Goto the first child of the elxiki line at point.
Return point if successful. If unsuccessful, return nil and don't
move point."
  (let ((start (point))
        (indent (elxiki-line-get-indentation)))
    (when (elxiki-line-valid-p)
      (if (and (elxiki-line-valid-p)
               (elxiki-line-goto-next)
               (> (elxiki-line-get-indentation) indent))
          (point)
        (goto-char start)
        nil))))

(defun elxiki-line-goto-next-sibling ()
  "Goto the first sibling after elxiki line at point.
Return point if successful. If unsuccessful, return nil and don't
move point."
  (let ((indent (elxiki-line-get-indentation))
        (real-start (point))
        (start (elxiki-line-goto-current)))
    (while (and (elxiki-line-goto-next)
                (> (elxiki-line-get-indentation) indent)))
    (if (and (elxiki-line-valid-p)
             (not (elxiki-line-blank-p))
             (= (elxiki-line-get-indentation) indent)
             (not (= start (point))))
        (point)
      (goto-char real-start)
      nil)))

(defun elxiki-line-goto-previous-sibling ()
  "Goto the first sibling before elxiki line at point.
Return nil if the sibling does not exist, or point is not at an
elxiki line."
  (let ((indent (elxiki-line-get-indentation))
        (real-start (point))
        (start (elxiki-line-goto-current)))
    (while (and (elxiki-line-goto-previous)
                (> (elxiki-line-get-indentation) indent)))
    (if (and (elxiki-line-valid-p)
             (not (elxiki-line-blank-p))
             (= (elxiki-line-get-indentation) indent)
             (not (= start (point))))
        (point)
      (goto-char real-start)
      nil)))

(defun elxiki-line-goto-nth-sibling (n)
  "Goto the nth sibling. Return point.
If unsuccessful, return nil and don't move point."
  (let ((start (point)))
    (when (elxiki-line-goto-current)
      (while (and (> n 0)
                  (elxiki-line-goto-next-sibling)
                  (setq n (1- n))))
      (while (and (< n 0)
                  (elxiki-line-goto-previous-sibling)
                  (setq n (1+ n))))
      (if (= n 0) 
          (point)
        (goto-char start)
        nil))))

(defun elxiki-line-goto-new-sibling ()
  "Goto the position where a new sibling should be inserted.
If successful, return point. If unsuccessful, return nil and
don't move point. If this reaches the end of the buffer and
cannot continue, it will instead return 'end."
  (when (elxiki-line-goto-current)
    (let ((indent (elxiki-line-get-indentation)))
      (while (and (elxiki-line-goto-next)
                  (>= (elxiki-line-get-indentation) indent)))
      (cond ((< (elxiki-line-get-indentation) indent)
             (point))
            ((elxiki-change-line 1)
             (point))
            ('else
             (end-of-line)
             'end)))))

(defun elxiki-line-goto-end-of-children ()
  "Goto the line after your last child ends.
If successful, return point. If unsuccessful, return nil and don't
move point. If this reaches the end of the buffer and cannot
continue, it will instead return 'end."
  (when (elxiki-line-goto-current)
    (let ((start (point))
          (end (or (elxiki-line-goto-next-sibling)
                   (elxiki-line-goto-new-sibling))))
      (forward-line 0)
      (if (= start (point))
          (goto-char (point-max))
        (forward-line -1)
        (when (elxiki-line-blank-p)
          (while (elxiki-line-blank-p)
            (forward-line -1)))
        (forward-line 1)
        (point)))))

(defun elxiki-line-goto-first-sibling ()
  "Goto the first sibling in this group of siblings.
If successful, return point. If unsuccessful, return nil and
don't move point."
  (when (elxiki-line-goto-previous-sibling)
    (while (elxiki-line-goto-previous-sibling))
    (point)))

(defun elxiki-line-insert (string &optional indent)
  "Insert STRING at the line point, indenting to INDENT.
If STRING is multi-line, then all lines are indented relative to
each other. INDENT defaults to 0. Do nothing if STRING is empty
or nil. If successful, move to insert point and return the (start
end) region of the inserted text."
  (when (> (length string) 0)
    (forward-line 0)
    (let ((end (- (length string) 1)))
      (when (string-equal "\n" (substring string end))
        (setq string (substring string 0 end))))
    (insert "\n")
    (forward-line -1)
    (let ((start (point))
          end)
      (insert string)
      (elxiki-normalize-indentation start (point) indent 'no-error)
      (setq end (point))
      (goto-char start)
      (list start end))))

(defun elxiki-line-insert-after-children (string &optional indent)
  "Insert STRING after all children with relative indentation INDENT.
STRING is normalized so that the first line is treated as having
0 indentation. INDENT defaults to 0. If successful, move to
insert point and return the (start end) region of the inserted
text."
  (setq indent (+ (or indent 0) (elxiki-line-get-indentation)))
  (let ((start (point))
        (new (elxiki-line-goto-end-of-children)))
      (if new
          (progn (when (not (= ?\n (preceding-char)))
                   (insert "\n"))
                 (elxiki-line-insert string indent))
        (goto-char start)
        nil)))

(defun elxiki-line-insert-after-siblings (string &optional indent)
  "Insert STRING after all siblings with relative indentation INDENT.
STRING is normalized so that the first line is treated as having
0 indentation. INDENT defaults to 0. If successful, move to
insert point and return the (start end) region of the inserted
text."
  (setq indent (+ (or indent 0) (elxiki-line-get-indentation)))
  (let ((start (point))
        (new (elxiki-line-goto-new-sibling)))
      (if new
          (progn (when (eq 'end new)
                   (insert "\n"))
                 (elxiki-line-insert string indent))
        (goto-char start)
        nil)))

(defun elxiki-line-goto-sibling (name &optional before)
  "Goto the sibling of the elxiki line at point matching NAME.
NAME is a string to match against sibling names. If successful,
return point. If unsuccessful, return nil and do not move
point. If BEFORE is non-nil, this will also check siblings before
point."
  (when (elxiki-line-valid-p)
    (let ((start (point))
          (name-regexp (regexp-quote name)))
      ;; Check siblings after first.
      (while (and (save-excursion (elxiki-line-goto-name)
                                  (not (looking-at-p name-regexp)))
                  (numberp (elxiki-line-goto-next-sibling))))
      (if (save-excursion (elxiki-line-goto-name)
                          (looking-at-p name-regexp))
          (point)
        (goto-char start)
        ;; Then check siblings before.
        (when before
          (while (and (elxiki-line-goto-previous-sibling)
                      (save-excursion (elxiki-line-goto-name)
                                      (not (looking-at-p name-regexp)))))
          (if (save-excursion (elxiki-line-goto-name)
                              (looking-at-p name-regexp))
              (point)
            (goto-char start)
            nil))))))

(defun elxiki-line-goto-child (name)
  "Goto the child of the elxiki line at point matching NAME.
NAME is a string to match against child names. If successful,
return point. If unsuccessful, return nil and do not move point."
  (let ((start (point)))
    (when (elxiki-line-goto-first-child)
      (let ((result (elxiki-line-goto-sibling name)))
        (if result
            result
          (goto-char start)
          nil)))))

(defun elxiki-line-follow-route (route &optional create)
  "Follow ROUTE through the current tree.
ROUTE is a list of strings to match line names against. The first
ROUTE item is used to find a sibling, and then each later ROUTE
item is used to find a child. If CREATE is non-nil, then create
elxiki lines you are trying to find that don't exist."
  (when (elxiki-line-valid-p)
    (let ((start (point)))
      (catch 'route
        (while route
          (unless (elxiki-line-goto-sibling (car route))
            (if create
                (elxiki-line-insert-after-siblings 
                 (concat "- " (car route)) elxiki-line-indent-count)
              (goto-char start)
              (throw 'route nil)))
          (if (setq route (cdr route))
              (unless (elxiki-line-goto-child (car route))
                (if create
                    (progn
                      (elxiki-line-insert-after-children 
                       (concat "- " (car route)) elxiki-line-indent-count)
                      (elxiki-line-goto-first-child))
                  (goto-char start)
                  (throw 'route nil)))
            (throw 'route (point))))))))

(defun elxiki-line-get-ancestry ()
  "Gets the ancestry of elxiki line at point.
This is a list of (prefix . name) cells, starting with the
topmost parent and ending with this line. Return nil if not on a
valid line."
  (save-excursion
    (let (ancestry prefix name)
      (while (and (progn (setq prefix (elxiki-line-get-prefix))
                         (setq name (elxiki-line-get-name)))
                  (or (elxiki-line-valid-p)
                      (elxiki-line-blank-p))
                  (setq ancestry (cons (list prefix name) ancestry))
                  (elxiki-line-goto-parent)))
      ancestry)))

(defun elxiki-line-get-branch-region ()
  "Return the (start end) region of the elxiki branch at point.
Return nil if there is no branch."
  (when (elxiki-line-valid-p)
    (save-excursion
      (forward-line 0)
      (let ((start (point)))
        (or (elxiki-line-goto-next-sibling)
            (elxiki-line-goto-new-sibling))
        (list start (point))))))

(defun elxiki-line-get-children-region ()
  "Return the (start end) region of the elxiki branch at point.
Return nil if there is no branch."
  (save-excursion
    (when (elxiki-line-goto-first-child)
      (let ((start (point)))
        (elxiki-line-goto-new-sibling)
        (list start (point))))))

(defun elxiki-line-get-siblings-region ()
  "Get the (start end) region of this elxiki line and all its siblings.
Return nil if not on an elxiki line."
  (save-excursion
    (when (elxiki-line-valid-p)
      (while (elxiki-line-goto-previous-sibling))
      (forward-line 0)
      (let ((start (point)))
        (elxiki-line-goto-new-sibling)
        (list start (point))))))

(defun elxiki-line-get-above-siblings-region ()
  "Get the (start end) region of this elxiki of all silbings above this line.
Return nil if not on an elxiki line, or there are no siblings above."
  (save-excursion
    (let ((end (elxiki-line-goto-current)))
      (when (elxiki-line-goto-previous-sibling)
        (while (elxiki-line-goto-previous-sibling))
        (list (point) end)))))

(defun elxiki-line-get-below-siblings-region ()
  "Get the (start end) region of this elxiki of all silbings below this line.
Return nil if not on an elxiki line, or there are no siblings below."
  (let ((start (elxiki-line-goto-next-sibling))
        (end (elxiki-line-goto-new-sibling)))
    (when (eq 'end end) (setq end (point-max)))
    (when (and start end)
      (list start end))))

(defun elxiki-line-delete-branch ()
  "Delete the elxiki line at point and its children."
  (let ((region (elxiki-line-get-branch-region)))
    (when region
      (apply 'delete-region region))))

(defun elxiki-line-delete-children ()
  "Delete the elxiki line at point's children."
  (let ((region (elxiki-line-get-children-region)))
    (when region
      (apply 'delete-region region))))

(defun elxiki-line-delete-siblings ()
  "Delete the siblings of elxiki line at point."
  (save-excursion
    (let ((above (elxiki-line-get-above-siblings-region))
          (below (elxiki-line-get-below-siblings-region)))
      (when below (apply 'delete-region below))
      (when above (apply 'delete-region above)))))

(defun elxiki-line-set-prefix (&optional prefix)
  "Changes elxiki line at point to have PREFIX.
You may optionally ignore or set PREFIX as nil, which will be
interpreted as \"\"."
  (unless prefix (setq prefix ""))
  (save-excursion
    (let ((old-prefix (progn (elxiki-line-goto-prefix)
                             (elxiki-line-looking-at-prefix))))
      (when old-prefix
        (delete-region (point)
                       (min (point-max) (+ (point) (length old-prefix)))))
      (insert prefix))))

(defun elxiki-line-doto-siblings (function &optional before)
  "Perform FUNCTION with the point at the start of each sibling.
If BEFORE is non-nil, then start at the first sibling instead of
at the sibling at point."
  (save-excursion
    (when (if before (elxiki-line-goto-first-sibling)
            (elxiki-line-goto-current))
      (funcall function)
      (while (elxiki-line-goto-next-sibling)
        (funcall function)))))

(defun elxiki-line-append-children (children &optional line-function)
  "Add CHILDREN underneath of the elxiki line at point, properly indented.
CHILDREN should be a (multiline) string, or a list of strings. If
LINE-FUNCTION is non-nil, it is called at every line of CHILDREN
after they have been inserted, at the base indentation level."
  (save-excursion
    (when (elxiki-line-goto-current)
      (when (listp children)
        (setq children (mapconcat 'identity children "\n")))
      (let ((indent (+ (elxiki-line-get-indentation)
                       elxiki-line-indent-count))
            (region (elxiki-line-insert-after-children
                     children elxiki-line-indent-count)))
        (when (and line-function region)
          (let ((line-function-2
                 (lambda ()
                   (forward-char indent)
                   (funcall line-function))))
            (apply 'elxiki-doto-lines line-function-2 region)))))))

(defun elxiki-line-match-siblings (predicate &optional first)
  "Return a list of offsets for siblings which match PREDICATE.
PREDICATE is a function of no arguments which is run when point
is at the start of the sibling. When FIRST is non-nil, start
counting at the first sibling, instead of the current one."
  (let ((offset 0)
        matches)
    (elxiki-line-doto-siblings
     (lambda ()
       (when (funcall predicate)
         (setq matches (cons offset matches)))
       (setq offset (1+ offset)))
     first)
    (nreverse matches)))

(defun elxiki-line-filter-siblings (predicate &optional restrict-none before)
  "Remove all siblings who do not satisfy PREDICATE.
PREDICATE is run when point is at the start of the sibling. if
RESTRICT-NONE is non-nil, then do not filter if it would result
in no siblings left. If BEFORE is non-nil, then also filter
siblings before the current line as well."
  (save-excursion
    (when (if before (elxiki-line-goto-first-sibling)
            (elxiki-line-goto-current))
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
            (if (elxiki-line-goto-next-sibling)
                (setq matches (cdr matches))
              (setq matches nil)
              (elxiki-line-goto-new-sibling)))
          (when (>= (current-indentation) indent)
            (let ((region (elxiki-line-get-below-siblings-region)))
              (when region (apply 'delete-region region)))))
         ((not restrict-none)
          (elxiki-line-delete-siblings)))))))

(defun elxiki-line-filter-children (predicate &optional restrict-none)
  "Remove all children who do not satisfy PREDICATE.
PREDICATE is run when point is at the start of the child. if
RESTRICT-NONE is non-nil, then do not filter if it would result
in no siblings left."
  (save-excursion
    (when (elxiki-line-goto-first-child)
      (elxiki-line-filter-siblings predicate restrict-none))))

(defun elxiki-line-promote ()
  "Promotes the current elxiki line and children to its parent's level.
Return non-nil if successful."
  (save-excursion
    (save-restriction
      (let ((region (elxiki-line-get-branch-region))
            (indent (elxiki-line-get-indentation))
            remove-count)
        (when (elxiki-line-goto-parent)
          (setq indent (- (elxiki-line-get-indentation) indent))
          (apply 'narrow-to-region region)
          (goto-char (point-min))
          (while (not (eobp))
            (elxiki-line-goto-beginning)
            (elxiki-change-indentation indent 'no-error)
            (forward-line 1))
          t)))))

(defun elxiki-line-replace-parent ()
  "Replace parent with current elxiki line.
Return point, or nil if the operation fails."
  (save-excursion
    (let ((parent (save-excursion (elxiki-line-goto-parent)))
          this-region this)
      (when parent
        (elxiki-line-promote)
        (setq this-region (elxiki-line-get-branch-region))
        (setq this (apply 'buffer-substring this-region))
        (apply 'delete-region this-region)
        (goto-char parent)
        (elxiki-line-delete-branch)
        (save-excursion (insert this))
        (point)))))

(provide 'elxiki-line)
;;; elxiki-line.el ends here
