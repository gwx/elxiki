;;; elxiki-command.el --- User actions for elxiki to perform.

;;; Commentary:
;; Defines the different actions that `elxiki-command' might take.

(require 'elxiki-line)

;;; Code:
(defvar elxiki-command-list nil
  "A list of commands to be tried as an action for the current elxiki line.
Each command is passed the line's context.  They are tried in
sequence until one returns non-nil.")

(defmacro define-elxiki-command (name &rest forms)
  "Create a command NAME to be run by `elxiki-command'.
Commands will be passed `context', and should return nil in
situations where they do not want to act."
  (declare (indent defun))
  `(progn
     (defun ,name (context &rest args) ,@forms)
     (add-to-list 'elxiki-command-list ',name)))

(let ((regex (rx "(" (group "define-elxiki-command")
                 (+ blank) (group (+ (not (any ")" blank)))))))
  (font-lock-add-keywords 'emacs-lisp-mode
                          `((,regex . 1)
                            (,regex 2 font-lock-function-name-face))))

(define-elxiki-command elxiki-command/collapse
  "Collapses anything with children."
  (when (elxiki-line-find-child)
    (when (string-equal "- " (elxiki-line-get-prefix))
      (elxiki-line-set-prefix "+ "))
    (apply 'delete-region (elxiki-line-find-all-children))
    t))

(define-elxiki-command elxiki-command/expand-dir
  "Expands a directory."
  (let ((dir (elxiki-context-default-directory context)))
    (when (and dir
               (member (elxiki-line-get-prefix)
                       '(nil "+ "))
               (file-directory-p dir)
               (not (elxiki-line-find-child)))
      (setq dir (file-name-as-directory dir))
      (elxiki-line-set-prefix "- ")
      (elxiki-line-add-children
       (mapcar
        (lambda (file)
          (let ((full (concat dir file)))
            (cond ((file-directory-p full)
                   (concat "+ " file "/"))
                  ((file-executable-p full)
                   (concat "$ ./" file))
                  ('else (concat "* " file)))))
        (directory-files dir)))
      t)))

(define-elxiki-command elxiki-command/expand-shell
  "Expands a shell process."
  (when (and (string-equal "$ " (elxiki-context-get-prefix context))
             (not (elxiki-line-find-child)))
    (elxiki-line-add-children
     (mapcar
      (lambda (line) (concat "| " line))
      (let ((default-directory (elxiki-context-default-directory context))
            (shell-file-name "/bin/bash"))
        (split-string (shell-command-to-string (elxiki-context-get-name context))
                      "\n"
                      'nonull))))
    (message "Ran: %s" (elxiki-context-get-name context))
    t))

(define-elxiki-command elxiki-command/async-shell
  "Opens up an asynchronous shell."
  (when (string-equal "% " (elxiki-context-get-prefix context))
    (let ((default-directory (elxiki-context-default-directory context))
          (shell-file-name "/bin/sh"))
      (async-shell-command (elxiki-context-get-name context)))
    t))

(define-elxiki-command elxiki-command/find-file
  "Finds a file."
  (when (string-equal "* " (elxiki-context-get-prefix context))
      (find-file (concat (elxiki-context-default-directory context)
                         (elxiki-context-get-name context)))
      t))

(define-elxiki-command elxiki-command/run-code
  "Runs elisp code."
  (let ((default-directory (elxiki-context-default-directory context)))
    (when (string-equal "! " (elxiki-context-get-prefix context))
      (elxiki-line-add-children
       (mapcar
        (lambda (line) (concat "| " line))
        (split-string
         (pprint-to-string
          (eval (read (elxiki-context-get-name context))))
         "\n"
         'nonull)))
      t)))

(provide 'elxiki-command)
;;; elxiki-command.el ends here
