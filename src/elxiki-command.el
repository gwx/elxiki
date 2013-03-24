;;; elxiki-command.el --- User actions for elxiki to perform.

;;; Commentary:
;; Defines the different actions that `elxiki-command' might take.

(require 'elxiki-line)
(require 'elxiki-context)
(require 'elxiki-menu)
(require 'elxiki-filter)

;;; Code:
(defvar elxiki-commands (cons nil nil)
  "A list of alists of predicates to elxiki commands to perform.
Each predicate and command should take the current context alist
as its single argument.  When `elxiki-command' is called, each
predicate is tried in turn, until one passes. The associated
command is then used. The index of the alist corresponds to the
number of C-u presses.")

(defun elxiki-command-register (command predicate &optional prefix-count low-priority)
  "Registers COMMAND to be performed by `elxiki-command' when PREDICATE passes.
PREDICATE and COMMAND should be a function of a single argument,
taking an elxiki context.

PREDICATE should return non-nil if you want COMMAND to be
run. Only the first command that succeeds is used.

PREFIX-COUNT is how many C-u presses you want to assign the
command to. It defaults to 0.

If LOW-PRIORITY is non-nil, then the command is appended to
`elxki-command-alist' instead of prepended, giving it a lower
precedence than other commands."
  (unless prefix-count (setq prefix-count 0))
  (let ((commands elxiki-commands)
        (cell (cons predicate command)))
    (while (> prefix-count 0)
      ;; Append an empty alist if needed.
      (when (null (cdr commands))
        (setcdr commands (cons nil nil)))
      (setq commands (cdr commands))
      (setq prefix-count (1- prefix-count)))
    (unless (member cell (car commands))
      (if low-priority
          (nconc (car commands) (list cell))
        (setcar commands
                (cons (cons predicate command)
                      (car commands)))))))

(defun elxiki-command-unregister (command)
  "Unregisters COMMAND from being used by `elxiki-command'."
  (let ((commands elxiki-commands))
    (while commands
      (setcar commands
              (rassq-delete-all command (car commands)))
      (setq commands (cdr commands)))))

(defun elxiki-command (&optional arg)
  "Perform the proper elxiki command at point."
  (interactive "P")
  (elxiki-filter -1)
  (when (elxiki-line-valid-p)
    (let ((commands elxiki-commands)
          (context (elxiki-context-from-ancestry (elxiki-line-get-ancestry))))
      (when (consp arg)
        (setq arg (car arg))
        (while (> arg 1)
          (setq commands (cdr commands))
          (setq arg (/ arg 4))))
      (setq commands (car commands))
      (while commands
        (if (funcall (caar commands) context)
            (progn (funcall (cdar commands) context)
                   (setq commands nil))
          (setq commands (cdr commands)))))))

(defun elxiki-command-no-filter (&optional arg)
  "Perform `elxiki-command' at point, suppressing filtering."
  (interactive "P")
  (let ((elxiki-filter-inhibit t))
    (elxiki-command arg)))

;;; Builtin Commands:

(defun elxiki-command-fold-p (context)
  "Return non-nil if the line at point should fold."
  (save-excursion (elxiki-line-goto-first-child)))

(defun elxiki-command/fold (context)
  "Remove all children from the elxiki line.
If the prefix is currently \"- \", change it to \"+ \"."
  (when (string-equal "- " (elxiki-line-get-prefix))
    (elxiki-line-set-prefix "+ "))
  (elxiki-line-delete-children))

(elxiki-command-register 'elxiki-command/fold 'elxiki-command-fold-p)

(defun elxiki-command-directory-unfold-p (context)
  "If CONTEXT a directory that can be unfolded."
  (and (not (save-excursion (elxiki-line-goto-first-child)))
       (member (elxiki-context-get-prefix context) '("+ " "- " nil))
       (eq 'directory (elxiki-context-get-type context))))

(defun elxiki-command/modify-directory-line ()
  "Modify the output of a directory line."
  (let ((absolute (expand-file-name (elxiki-line-get-name))))
    (elxiki-line-goto-prefix)
    (cond ((file-directory-p absolute)
           (insert "+ ")
           (end-of-line)
           (insert "/"))
          ((file-executable-p absolute)
           (insert "$ ./"))
          ('else
           (insert "& ")))))

(defun elxiki-command/unfold-directory (context)
  "Adds the directory's files as children lines."
  (elxiki-line-set-prefix "- ")
  (let* ((default-directory (elxiki-context-get-directory context)))
    (elxiki-line-append-children
     (directory-files default-directory)
     'elxiki-command/modify-directory-line)
    (elxiki-filter)))

(defun elxiki-command/unfold-directory-unhidden (context)
  "Adds the directory's files (ignoring hidden) as children lines."
  (elxiki-line-set-prefix "- ")
  (let* ((default-directory (elxiki-context-get-directory context))
         (regex (rx string-start (not (any ".")))))
    (elxiki-line-append-children
     (directory-files default-directory nil regex)
     'elxiki-command/modify-directory-line)
    (elxiki-filter)))

(elxiki-command-register 'elxiki-command/unfold-directory-unhidden
                         'elxiki-command-directory-unfold-p)

(elxiki-command-register 'elxiki-command/unfold-directory
                         'elxiki-command-directory-unfold-p
                         1)

(defun elxiki-command-shell-unfold-p (context)
  "If CONTEXT indicates a shell command that can be unfolded."
  (and (not (save-excursion (elxiki-line-goto-first-child)))
       (string-equal "$ " (elxiki-context-get-prefix context))))

(defun elxiki-command/unfold-shell (context)
  "Adds the output of the shell command as children lines."
  (let ((default-directory (elxiki-context-get-directory context))
        (name (elxiki-context-get-name context))
        (line-prepare (lambda () (insert "| "))))
    (elxiki-line-append-children (shell-command-to-string name) line-prepare)
    (message "Ran: %s" name)
    (elxiki-filter)))

(elxiki-command-register 'elxiki-command/unfold-shell
                         'elxiki-command-shell-unfold-p)

(defun elxiki-command-async-shell-p (context)
  "If CONTEXT indicates a shell command that can be unfolded."
  (and (not (save-excursion (elxiki-line-goto-first-child)))
       (string-equal "% " (elxiki-context-get-prefix context))))

(defun elxiki-command/run-async (context)
  "Runs the elxiki line asynchronously."
  (let ((default-directory (elxiki-context-get-directory context))
        (name (elxiki-context-get-name context)))
    (async-shell-command name)))

(elxiki-command-register 'elxiki-command/run-async
                         'elxiki-command-async-shell-p)

(defun elxiki-command-find-p (context)
  "If CONTEXT indicates a file to be found."
  (string-equal "& " (elxiki-context-get-prefix context)))

(defun elxiki-command/find-file (context)
  "Finds the file for the elxiki line."
  (let ((default-directory (elxiki-context-get-directory context))
        (name (elxiki-context-get-name context)))
    (find-file (expand-file-name name))))

(defun elxiki-command/find-file-other-window (context)
  "Finds the file for the elxiki line."
  (let ((default-directory (elxiki-context-get-directory context))
        (name (elxiki-context-get-name context)))
    (find-file-other-window (expand-file-name name))))

(defun elxiki-command/find-file-other-frame (context)
  "Finds the file for the elxiki line."
  (let ((default-directory (elxiki-context-get-directory context))
        (name (elxiki-context-get-name context)))
    (find-file-other-frame (expand-file-name name))))

(elxiki-command-register 'elxiki-command/find-file
                         'elxiki-command-find-p)

(elxiki-command-register 'elxiki-command/find-file-other-window
                         'elxiki-command-find-p
                         1)

(elxiki-command-register 'elxiki-command/find-file-other-frame
                         'elxiki-command-find-p
                         2)

(defun elxiki-command-emacs-lisp-p (context)
  "If CONTEXT indicates an emacs lisp command that can be unfolded."
  (and (not (save-excursion (elxiki-line-goto-first-child)))
       (string-equal "! " (elxiki-context-get-prefix context))))

(defun elxiki-command/unfold-emacs-lisp (context)
  "Adds the output of the emacs lisp command as children lines."
  (let ((default-directory (elxiki-context-get-directory context))
        (name (elxiki-context-get-name context))
        (line-prepare (lambda () (insert "| "))))
    (elxiki-line-append-children (format "%S" (eval (read name)))
                                 line-prepare)
    (elxiki-filter)))

(elxiki-command-register 'elxiki-command/unfold-emacs-lisp
                         'elxiki-command-emacs-lisp-p)

(defun elxiki-command-silent-emacs-lisp-p (context)
  "If CONTEXT indicates a silent emacs lisp command that can be run."
  (and (not (save-excursion (elxiki-line-goto-first-child)))
       (string-equal "!! " (elxiki-context-get-prefix context))))

(defun elxiki-command/silent-run-emacs-lisp (context)
  "Runs an emacs lisp command."
  (let ((default-directory (elxiki-context-get-directory context))
        (name (elxiki-context-get-name context)))
    (eval (read name))))

(elxiki-command-register 'elxiki-command/silent-run-emacs-lisp
                         'elxiki-command-silent-emacs-lisp-p)

(defun elxiki-command-menu-act-p (context)
  "If CONTEXT indicates a menu to act upon."
  (and (not (save-excursion (elxiki-line-goto-first-child)))
       (eq 'menu (elxiki-context-get-type context))))

(defun elxiki-command/menu-act (context)
  "Performs the given menu action."
  (let ((default-directory (elxiki-context-get-directory context))
        (prefix (elxiki-context-get-prefix context)))
    (when (string-equal "+ " prefix)
      (elxiki-line-set-prefix "- "))
    (when (and (= 0 (length prefix))
               (elxiki-menu-find (elxiki/path-root
                                  (elxiki-context-get-menu context))))
      (if (elxiki-context-menu-root-p context)
          (elxiki-line-set-prefix "@ ")
        (elxiki-line-set-prefix "+ ")))
    (elxiki-line-append-children (elxiki-menu-act context))
    (elxiki-filter)))

(elxiki-command-register 'elxiki-command/menu-act
                         'elxiki-command-menu-act-p)

(provide 'elxiki-command)

;;; elxiki-command.el ends here
