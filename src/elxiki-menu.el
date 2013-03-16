;;; elxiki-menu.el --- Create menus with various commands.

(require 'elxiki-util)
(require 'elxiki-line)
(require 'elxiki-context)

(defvar elxiki-menu-directory (expand-file-name "~/.emacs.d/elxiki-menus/")
  "Where elxiki keeps its menu definitions.
Must end in a /.")

(defvar elxiki-menu-install-directory
  (concat elxiki-directory "menu/")
  "Where the default elxiki menus are held.")

(defvar elxiki-menu-path
  (list elxiki-menu-directory elxiki-menu-install-directory)
  "Path of directories to check for menu files.")

(defvar elxiki-menu-buffer-name-prefix " elxiki-menu-"
  "Prefix to name buffers holding elxiki menus with.")

(defun elxiki-menu-find (menu)
  "Search `elxiki-menu-path' for MENU.
Checks for MENU.menu and MENU.menu.el in all folders in path."
  (let ((menu-path elxiki-menu-path)
        file)
    (while (and menu-path (not file))
      (let ((name (concat (car menu-path) menu ".menu")))
        (if (file-exists-p name)
            (setq file name)
          (setq name (concat name ".el"))
          (if (file-exists-p name)
              (setq file name)
            (setq menu-path (cdr menu-path))))))
    file))

(defun elxiki-menu-load (context &optional force)
  "Load the menu described by CONTEXT so it is ready to process.
Return a description of the menu. If FORCE is non-nil, load an
empty menu if it does not exist."
  (let* ((root-menu-name (elxiki/path-root (elxiki-context-get-menu context)))
         (menu-file (elxiki-menu-find root-menu-name))
         buffer)
    (cond
     (menu-file
      (setq buffer (generate-new-buffer 
                    (concat elxiki-menu-buffer-name-prefix
                            root-menu-name)))
      (cond
       ;; .menu file (text only)
       ((string-equal "menu" (file-name-extension menu-file))
        (with-current-buffer buffer
          (insert-file-contents menu-file))
        (list buffer nil menu-file))
       ;; .el file (functions)
       ((string-equal "el" (file-name-extension menu-file))
        (let (*elxiki-menu-functions* text-fun)
          (load menu-file t t)
          (setq text-fun (cdr (assoc (rx "_init" string-end)
                                     *elxiki-menu-functions*)))
          (when text-fun
            (with-current-buffer buffer
              (insert (apply text-fun context))))
          (list buffer *elxiki-menu-functions* menu-file)))))
     (force
      (list (generate-new-buffer elxiki-menu-buffer-name-prefix) 
            nil
            (concat (file-name-as-directory elxiki-menu-directory)
                    (concat root-menu-name ".menu")))))))

(defun elxiki-menu--reduce-item-name (menu-item)
  "Convert MENU-ITEM to the canonical function name.
Strips out spaces."
  (setq menu-item (elxiki/strip-slash menu-item))
  (replace-regexp-in-string " " "" menu-item))

(defun elxiki-menu--item-to-regexp (item)
  "Convert ITEM name to a regexp matching that item."
  (let ((result item))
    (setq result (concat (regexp-quote (elxiki-menu--reduce-item-name result))
                         (rx string-end)))
    (setq result
          (replace-regexp-in-string (rx (? "/") (group "_any") (? "/"))
                                    (rx (* (not (any "/"))))
                                    result nil nil 1))
    (replace-regexp-in-string (rx (? "/") (group "_many") (? "/"))
                              (rx (* nonl))
                              result nil nil 1)))

(defmacro defmenu (name &rest body)
  "Define a menu item. For use in .menu.el files.

When a menu item is opened, if there is a method attached to that
name, then that method is called, and the resulting string is
what is attached under the item being opened. Menus are named
according to their full path, with spaces stripped. For example:

@ Menu/
  + Start/
    + Item A/
    + Item B/

'Item A' can be defined with (defmenu Start/ItemA ...).

There are several special names you can use in the path:

* _root is the root of the menu. (defmenu A ...) will match
  /Menu/B/A as well as /Menu/A, but (defmenu _root/A ...) will
  only match the latter.
* _any stands for any 1 menu item. For instance, 
  (defmenu A/_any/A ...) will match /A/B/A but not /A/B/B/A.
* _many stands for 1 or more menu items. For instance,
  (defmenu A/_many ...) will match /A/B and /A/B/C, but not
  /A by itself.

Later defmenu calls in a file take precedence.

There is also the special menu _init, which is called every time
any item in the hierarchy is opened. Its return value is used as
the text for the menu. The only time you may skip defining this
is when every single item in the menu has an associated action.

The menu body is passed the following arguments:
* prefix :: The prefix for the line being called.
* name :: The rest of the line being called.
* directory :: The directory the menu is being run under.
* menu :: The full menu path being called.
* type :: Should always be 'menu."
  (declare (indent defun))
  `(setq *elxiki-menu-functions*
         (cons (cons (elxiki-menu--item-to-regexp (symbol-name ',name))
                     (lambda ,elxiki-context-format ,@body))
               *elxiki-menu-functions*)))

(let ((regex (rx "(" (group "defmenu")
                 blank (group (+ (not (any blank "\n")))))))
  (font-lock-add-keywords 'emacs-lisp-mode
                          `((,regex . 1)
                            (,regex 2 'font-lock-function-name-face))))

(defun elxiki-menu-get-buffer (menu)
  "Return the buffer from MENU."
  (car menu))

(defun elxiki-menu-dispose (menu)
  "Kills the buffer associated with MENU."
  (kill-buffer (elxiki-menu-get-buffer menu)))

(defun elxiki-menu-get-action (menu context)
  "Return from MENU the function CONTEXT calls for.
See `defmenu' documentation for how actions are specified."
  (save-match-data
    (let ((path (elxiki/strip-slash (elxiki-context-get-menu context)))
          (actions (nth 1 menu))
          action)
      ;; Prepare path
      (setq path
            (replace-regexp-in-string (rx string-start (group (*? any)) 
                                          (or "/" string-end))
                                      "_root" path nil nil 1))
      (setq path (elxiki-menu--reduce-item-name path))
      ;; Find the first matching action.
      (while actions
        (if (string-match (caar actions) path)
            (progn (setq action (cdar actions))
                   (setq actions nil))
          (setq actions (cdr actions))))
      action)))

(defun elxiki-menu-get-file (menu)
  "Return the file backing MENU."
  (nth 2 menu))

(defun elxiki-menu/fold-if-closed-prefix ()
  (let ((context (elxiki-context-from-ancestry
                  (elxiki-line-get-ancestry))))
    (when (string-equal "+ " (elxiki-context-get-prefix context))
      (elxiki-line-delete-children))))

(defun elxiki-menu/prepare (result)
  "Prepare a section of text for insertion.
This involves aligning it and folding all children."
  (when result
    (let (pos)
      (with-temp-buffer
        (insert result)
        (elxiki/normalize-buffer-indentation)
        (goto-char (point-min))
        (elxiki-menu/fold-if-closed-prefix)
        (while (elxiki-line-goto-next)
          (elxiki-menu/fold-if-closed-prefix))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun elxiki-menu-act (context)
  "Do the menu action described by CONTEXT and return the resulting text."
  (let* ((menu (elxiki-menu-load context))
         (inner-path (elxiki-drop-root (elxiki-context-get-menu context)))
         (action (elxiki-menu-get-action menu context))
         result)
    (unwind-protect
        (when menu
          (cond
           ;; If there is a matching action, do it.
           (action
            (setq result (apply action context)))
           ;; Treat the root menu specially.
           ((string-equal "" inner-path)
            (setq result
                  (with-current-buffer (elxiki-menu-get-buffer menu)
                    (buffer-substring-no-properties (point-min) (point-max)))))
           ;; No function, so just read from buffer.
           ('else
            (with-current-buffer (elxiki-menu-get-buffer menu)
              (goto-char (point-min))
              (elxiki-line-follow-route (split-string inner-path "/"))
              (let ((children (elxiki-line-get-children-region)))
                (when children
                  (setq result
                        (apply 'buffer-substring-no-properties children)))))))
          (elxiki-menu/prepare result))
      (when menu
        (elxiki-menu-dispose menu)))))

(defun elxiki-menu-edit ()
  "Edit the menu at point.
If the menu is a builtin menu, copy it to your personal directory
before editing."
  (interactive)
  (let* ((context (elxiki-context-from-ancestry (elxiki-line-get-ancestry)))
         (menu (elxiki-menu-load context 'force)))
    (unwind-protect
        (let ((file (elxiki-menu-get-file menu)))
          (if (string-equal elxiki-menu-directory (file-name-directory file))
              (find-file file)
            (find-file (concat elxiki-menu-directory
                               (file-name-nondirectory file)))
            (insert-file-contents file)))
      (elxiki-menu-dispose menu))))

;; (defun elxiki-menu-save ()
;;   "Save the (sub)menu at point."
;;   (interactive)
;;   (let* ((context (elxiki-context-from-ancestry (elxiki-line-get-ancestry)))
;;          (menu (elxiki-menu-load context 'force))
;;          (inner-path (elxiki-drop-root (elxiki-context-get-menu context)))
;;          (children (elxiki-line-find-all-children))
;;          (buffer (elxiki-menu-get-buffer menu))
;;          start)
;;     (unwind-protect
;;         (save-restriction
;;           (save-excursion
;;             (if (string-equal (file-name-extension (elxiki-menu-get-file menu))
;;                               "el")
;;                 (message "Can't save menus with code.")
;;               (when children
;;                 (apply 'narrow-to-region children))
;;               (goto-char (point-min))
;;               (with-current-buffer buffer
;;                 (goto-char (point-min))
;;                 (apply 'narrow-to-region (elxiki-line-find-self)))
;;               (while (not (= (point-max) (point)))
;;                 (if (elxiki-line-find-first-child

;;             (if (= 0 (length inner-path))
;;                 (progn
;;                   (delete-region (point-min) (point-max))
;;                   (mapc (lambda (string) (insert string "\n"))
;;                         (elxiki/normalize-indentation
;;                          (split-string children "\n"))))
;;               (elxiki-line-goto-route inner-path 'create)
;;               (elxiki-line-fold)
;;               (elxiki-line-add-children children))
;;             (write-region nil nil (elxiki-menu-get-file menu) 
;;                           nil 'no-message))
;;           (message "Saved Menu: %s" (elxiki-context-get-menu context)))
;;       (elxiki-menu-dispose menu))))

(defun elxiki-menu-all ()
  "Return a list of all menu names."
  (let ((regex (rx string-start (group (+ any)) ".menu" (? ".el") string-end))
        (path elxiki-menu-path)
        files
        menus)
    (while path
      (setq files (directory-files (car path) nil regex))
      (while files
        (string-match regex (car files))
        (setq menus (cons (match-string 1 (car files)) menus))
        (setq files (cdr files)))
      (setq path (cdr path)))
    menus))

(provide 'elxiki-menu)

;;; elxiki-menu.el ends here
