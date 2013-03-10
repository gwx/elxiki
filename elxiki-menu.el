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

(defvar elxiki-menu-buffer-name-prefix " elxiki-menu"
  "Prefix to name buffers holding elxiki menus with.")

(defun elxiki-menu-find (menu)
  "Search `elxiki-menu-path' for MENU.
Checks for MENU.menu and MENU.menu.el in all folders in path."
  (let ((menu-path elxiki-menu-path)
        file)
    (while (and menu-path (not file))
      (let ((name (concat (car menu-path) root-menu-name ".menu")))
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
      (setq buffer (generate-new-buffer elxiki-menu-buffer-name-prefix))
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
          (setq text-fun (cdr (assoc '_init *elxiki-menu-functions*)))
          (when text-fun
            (with-current-buffer buffer
              (insert (apply text-fun context))))
          (list buffer *elxiki-menu-functions* menu-file)))))
     (force
      (list (generate-new-buffer elxiki-menu-buffer-name-prefix) 
            nil
            (concat (file-name-as-directory elxiki-menu-directory)
                    (concat root-menu-name ".menu")))))))

(defmacro defmenu (name &rest body)
  "Define a menu item. For use in .menu.el files.

When a menu item is opened, if there is a method attached to that
name, then that method is called, and the resulting string is
what is attached under the item being opened. Menus are named
according to their full path, with spaces stripped. For example:

+ Start/
  + Item A/
  + Item B/

'Item A' can be defined with (defmenu Start/ItemA ...).

There are also several special menu names:
* _init is called every time any item in the hierarchy is
  opened. Its return value is used as the text for the menu.
* _root is called when the top level item is opened. Its return
  value will override that of _init. Use this if you want to
  define a simple menu command. For instance, see the 'zone'
  menu.
* _undefined is called whenever a menu item you don't have
  defined is opened.

The menu body is passed the following arguments:
* prefix :: The prefix for the line being called.
* name :: The rest of the line being called.
* directory :: The directory the menu is being run under.
* menu :: The full menu path being called.
* type :: Should always be 'menu."
  (declare (indent defun))
  `(setq *elxiki-menu-functions*
         (cons (cons ',name (lambda ,elxiki-context-format ,@body))
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

(defun elxiki-menu/to-function-name (menu-item)
  "Convert MENU-ITEM to the corresponding function name.
Strips out spaces."
  (intern
   (replace-regexp-in-string " " "" menu-item)))

(defun elxiki-menu-get-action (menu action)
  "Return from MENU the action with name ACTION."
  (cdr (assoc (elxiki-menu/to-function-name action)
              (cadr menu))))

(defun elxiki-menu-get-file (menu)
  "Return the file backing MENU."
  (nth 2 menu))

(defun elxiki-menu/fold-if-not-directory ()
  (let ((context (elxiki-context-from-ancestry
                             (elxiki-line-get-ancestry))))
    (unless (eq 'directory (elxiki-context-get-type context))
      (elxiki-line-fold))))

(defun elxiki-menu/prepare (string)
  "Prepare a section of text for insertion.
This involves aligning it and folding all children."
  (when string
    (let (pos)
      (with-temp-buffer
        (mapc (lambda (string) (insert string "\n"))
              (elxiki/normalize-indentation
               (split-string string "\n")))
        (elxiki-menu/fold-if-not-directory)
        (goto-char (point-min))
        (while (setq pos (elxiki-line-find-first-sibling))
          (goto-char pos)
          (elxiki-menu/fold-if-not-directory))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun elxiki-menu-act (context)
  "Do the menu action described by CONTEXT and return the resulting text."
  (let ((menu (elxiki-menu-load context))
        (inner-path (elxiki/drop-root (elxiki-context-get-menu context)))
        item-function result)
    (unwind-protect
        (when menu
          (setq item-function (elxiki-menu-get-action menu inner-path))
          (cond
           ;; Treat the root menu specially.
           ((string-equal "" inner-path)
            (setq item-function (elxiki-menu-get-action menu "_root"))
            (if item-function
                (setq result (apply item-function context))
              (setq result
                    (with-current-buffer (elxiki-menu-get-buffer menu)
                      (buffer-substring-no-properties (point-min) (point-max))))))
           ;; There is a function defined for this action, so do it.
           (item-function
            (setq result (apply item-function context)))
           ;; There is an _undefined action, so do that instead.
           ((setq item-function (elxiki-menu-get-action menu "_undefined"))
            (setq result (apply item-function context)))
           ;; No function, so just read from buffer.
           ('else
            (with-current-buffer (elxiki-menu-get-buffer menu)
              (goto-char (point-min))
              (elxiki-line-goto-route inner-path)
              (let ((children (elxiki-line-find-all-children)))
                (when children
                  (setq result
                        (apply 'buffer-substring-no-properties children)))))))
          (elxiki-menu/prepare result))
      (when menu
        (elxiki-menu-dispose menu)))))

(defun elxiki-menu-edit ()
  "Edit the menu at point."
  (interactive)
  (let* ((context (elxiki-context-from-ancestry (elxiki-line-get-ancestry)))
         (menu (elxiki-menu-load context 'force)))
    (unwind-protect
        (find-file (elxiki-menu-get-file menu))
      (elxiki-menu-dispose menu))))

;; (defun elxiki-menu-save ()
;;   "Save the (sub)menu at point."
;;   (interactive)
;;   (let* ((context (elxiki-context-from-ancestry (elxiki-line-get-ancestry)))
;;          (menu (elxiki-menu-load context 'force))
;;          (inner-path (elxiki/drop-root (elxiki-context-get-menu context)))
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
