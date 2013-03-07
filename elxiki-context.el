;;; elxiki-context.el --- How to interpret a given elxiki line.

;;; Commentary:
;; This is where elxiki interprets information about the elxiki
;; lines. To extend, take a look at `elxiki-context-functions' and
;; `elxiki-context-post-functions'.

;;; Code:
(defvar elxiki-context-functions 
  '(elxiki-context/join-function
    elxiki-context/file-function
    elxiki-context/menu-function)
  "A list of functions used to interpret context.
Each function takes thre arguments, the current context, the
current line prefix, and the current line name.  Functions are
tried in order; the context is set as the first non-nil result.")

(defun elxiki-context/file-function (context prefix name)
  "If this is the first line, see if it starts a file name."
  (and (not context)
       (member (string-to-char name) '(?. ?/ ?~))
       (list (cons 'file name))))

(defun elxiki-context/menu-function (context prefix name)
  "If this is the first line, label it as a menu item."
  (and (not context)
       (list (cons 'menu name))))

(defun elxiki-context/join-function (context prefix name)
  "Appends name to parent's file or menu as appropriate."
  (let ((cell (or (assoc 'file context)
                  (assoc 'menu context))))
    (when cell
      (setcdr cell
              (concat (file-name-as-directory (cdr cell))
                      name))
      context)))

(defvar elxiki-context-post-functions
  nil
  "A list of functions of one argument that are expected to take
the context resulting from the `elxiki-context-functions' and apply
a single, final transformation to them. Unlike
`elxiki-context-functions' all of these are applied.")

(defun elxiki-context-get (ancestry)
  "Takes a list of elxiki line prefixes and names and return a context alist.
ANCESTRY should be of the form that `elxiki-line-get-ancestry'
returns.  The results of this can be modified by adjusting
`elxiki-content-functions' and `elxiki-content-post-functions'."
  (let (context
        (post-functions elxiki-context-post-functions))
    ;; Loop through each ancestry line.
    (while ancestry
      (let ((functions elxiki-context-functions)
            interim)
        ;; Test each function against the ancestry entry.
        (while functions
          (setq interim (apply (car functions) context (car ancestry)))
          (when interim
            (setq context interim)
            (setq functions nil))
          (setq functions (cdr functions))))
      (setq ancestry (cdr ancestry)))
    ;; Loop through each post function
    (while post-functions
      (setq context (funcall (car post) context))
      (setq post-functions (cdr post-functions)))
    context))

(defun elxiki-context-get-dir (context)
  "Gets the current directory from CONTEXT."
  (file-name-directory (or (cdr (assoc 'file context))
                           default-directory)))

(provide 'elxiki-context)
;;; elxiki-context.el ends here
