;;; elxiki-command.el --- User actions for elxiki to perform.

;;; Commentary:
;; Defines the different actions that `elxiki-command' might take.

(require 'elxiki-line)

;;; Code:
(defvar elxiki-command-alist nil
  "An alist of predicates to elxiki commands to perform.
Each predicate and command should take the current context alist
as its single argument.  When `elxiki-command' is called, each
predicate is tried in turn, until one passes. The associated
command is then used.")

(defun elxiki-command-register (command predicate &optional low-priority)
  "Registers COMMAND to be performed by `elxiki-command' when PREDICATE passes.
PREDICATE and COMMAND should be a function of a single argument,
taking an elxiki context.

PREDICATE should return non-nil if you want COMMAND to be
run. Only the first command that succeeds is used. If
LOW-PRIORITY is non-nil, then the command is appended to
`elxki-command-alist' instead of prepended, giving it a lower
precedence than other commands."
  (add-to-list 'elxiki-command-alist (cons predicate command) low-priority))

(defun elxiki-command-unregister (command)
  "Unregisters COMMAND from being used by `elxiki-command'."
  (setq elxiki-command-alist (rassq-delete-all command elxiki-command-alist)))

(defun elxiki-command ()
  "Perform the proper elxiki command at point."
  (interactive)
  (when (elxiki-line-get)
    (let ((commands elxiki-command-alist)
          (context (elxiki-context-from-ancestry (elxiki-line-get-ancestry))))
      (while commands
        (if (funcall (caar commands) context)
            (progn (funcall (cdar commands) context)
                   (setq commands nil))
          (setq commands (cdr commands)))))))

;;; Builtin Commands:

(defun elxiki-command-children-p (context)
  "Return non-nil if the line at point has children."
  (elxiki-line-find-child))

(defun elxiki-command/fold (context)
  "Remove all children from the elxiki line.
If the prefix is currently \"- \", change it to \"+ \"."
  (when (string-equal "- " (elxiki-context-get-prefix context))
    (elxiki-line-set-prefix "+ "))
  (apply 'delete-region (elxiki-line-find-all-children)))

(elxiki-command-register 'elxiki-command/fold 'elxiki-command-children-p)

(defun elxiki-command-directory-unfold-p (context)
  "If CONTEXT a directory that can be unfolded."
  (and (not (elxiki-command-children-p context))
       (elxiki-context-directory-p context)))

(defun elxiki-command/unfold-directory (context)
  "Adds the directory's files as children lines."
  (elxiki-line-set-prefix "- ")
  (let* ((default-directory (elxiki-context-default-directory context))
         (line-prepare
          (lambda (line)
            (let ((absolute (expand-file-name line)))
              (cond ((file-directory-p absolute)
                     (concat "+ " line "/"))
                    ((file-executable-p absolute)
                     (concat "$ ./" line))
                    ('else
                     (concat "* " line)))))))
    (elxiki-line-add-children 
     (directory-files default-directory)
     line-prepare)))

(elxiki-command-register 'elxiki-command/unfold-directory
                         'elxiki-command-directory-unfold-p)

(defun elxiki-command-shell-unfold-p (context)
  "If CONTEXT indicates a shell command that can be unfolded."
  (and (not (elxiki-command-children-p context))
       (string-equal "$ " (elxiki-context-get-prefix context))))

(defun elxiki-command/unfold-shell (context)
  "Adds the output of the shell command as children lines."
  (let ((default-directory (elxiki-context-default-directory context))
        (shell-file-name "/bin/bash")
        (name (elxiki-context-get-name context))
        (line-prepare (lambda (line)
                        (concat "| " line))))
    (elxiki-line-add-children (shell-command-to-string name) line-prepare)
    (message "Ran: %s" name)))

(elxiki-command-register 'elxiki-command/unfold-shell
                         'elxiki-command-shell-unfold-p)

(defun elxiki-command-async-shell-p (context)
  "If CONTEXT indicates a shell command that can be unfolded."
  (and (not (elxiki-command-children-p context))
       (string-equal "% " (elxiki-context-get-prefix context))))

(defun elxiki-command/run-async (context)
  "Runs the elxiki line asynchronously."
  (let ((default-directory (elxiki-context-default-directory context))
        (shell-file-name "/bin/bash")
        (name (elxiki-context-get-name context)))
    (async-shell-command name)))

(elxiki-command-register 'elxiki-command/run-async
                         'elxiki-command-async-shell-p)

(defun elxiki-command-find-p (context)
  "If CONTEXT indicates a file to be found."
  (string-equal "* " (elxiki-context-get-prefix context)))

(defun elxiki-command/find-file (context)
  "Finds the file for the elxiki line."
  (let ((default-directory (elxiki-context-default-directory context))
        (name (elxiki-context-get-name context)))
    (find-file (expand-file-name name))))

(elxiki-command-register 'elxiki-command/find-file
                         'elxiki-command-find-p)

(defun elxiki-command-emacs-lisp-p (context)
  "If CONTEXT indicates an emacs lisp command that can be unfolded."
  (and (not (elxiki-command-children-p context))
       (string-equal "! " (elxiki-context-get-prefix context))))

(defun elxiki-command/unfold-emacs-lisp (context)
  "Adds the output of the emacs lisp command as children lines."
  (let ((default-directory (elxiki-context-default-directory context))
        (name (elxiki-context-get-name context))
        (line-prepare (lambda (line) (concat "| " line))))
    (elxiki-line-add-children (pprint-to-string (eval (read name))) 
                              line-prepare)))

(elxiki-command-register 'elxiki-command/unfold-emacs-lisp
                         'elxiki-command-emacs-lisp-p)

(provide 'elxiki-command)
;;; elxiki-command.el ends here
