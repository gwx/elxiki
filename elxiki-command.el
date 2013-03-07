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
  (let ((dir (cdr (assoc 'file context))))
    (when (and dir
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
                   (concat "$ " file))
                  ('else (concat "* " file)))))
        (directory-files dir)))
      t)))

(define-elxiki-command elxiki-command/expand-shell
  "Expands a shell process."
  (let ((line-info (elxiki-line-get)))
    (when (and (string-equal "$ " (nth 0 line-info))
               (not (elxiki-line-find-child)))
      (elxiki-line-add-children
       (mapcar
        (lambda (line) (concat "| " line))
        (split-string (shell-command-to-string
                       (concat "cd " (elxiki-context-get-dir context)
                               "; " (nth 1 line-info)))
                      "\n"
                      'nonull)))
      (message "Ran: %s" (nth 1 line-info))
      t)))

(define-elxiki-command elxiki-command/async-shell
  "Opens up an asynchronous shell."
  (let ((line-info (elxiki-line-get)))
    (when (string-equal "% " (nth 0 line-info))
      (async-shell-command
       (concat "cd " (elxiki-context-get-dir context)
               "; " (nth 1 line-info)))
      t)))

(define-elxiki-command elxiki-command/find-file
  "Finds a file."
  (let ((line-info (elxiki-line-get)))
    (when (string-equal "* " (nth 0 line-info))
      (find-file (concat (elxiki-context-get-dir context)
                         (nth 1 line-info)))
      t)))

(define-elxiki-command elxiki-command/run-code
  "Runs elisp code."
  (let ((line-info (elxiki-line-get)))
    (when (string-equal "! " (nth 0 line-info))
      (elxiki-line-add-children
       (mapcar
        (lambda (line) (concat "| " line))
        (split-string
         (pprint-to-string
          (eval (read (nth 1 line-info))))
         "\n"
         'nonull)))
      t)))

(provide 'elxiki-command)
;;; elxiki-command.el ends here
