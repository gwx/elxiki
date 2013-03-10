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

(defun elxiki-menu-load (context)
  "Load the menu described by CONTEXT so it is ready to process.
Return a description of the menu."
  (let* ((root-menu-name (elxiki/path-root (elxiki-context-get-menu context)))
         (menu-file (elxiki-menu-find root-menu-name))
         buffer)
    (when menu-file
      (setq buffer (generate-new-buffer elxiki-menu-buffer-name-prefix))
      (cond
       ;; .menu file (text only)
       ((string-equal "menu" (file-name-extension menu-file))
        (with-current-buffer buffer
          (insert-file-contents menu-file))
        (list buffer))
       ;; .el file (functions)
       ((string-equal "el" (file-name-extension menu-file))
        (let (*elxiki-menu-functions* text-fun)
          (load menu-file t t)
          (setq text-fun (cdr (assoc 'text *elxiki-menu-functions*)))
          (when text-fun
            (with-current-buffer buffer
              (insert (funcall text-fun context))))
          (list buffer *elxiki-menu-functions*)))))))

(defmacro defmenu (name &rest body)
  "Define a menu item. For use in .menu.el files."
  (declare (indent defun))
  `(setq *elxiki-menu-functions*
         (cons (cons ',name (lambda (context) ,@body))
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

(defun elxiki-menu/prepare (string)
  "Prepare a section of text for insertion.
This involves aligning it and folding all children."
  (when string
    (let (pos)
      (with-temp-buffer
        (mapc (lambda (string) (insert string "\n"))
              (elxiki/normalize-indentation
               (split-string string "\n")))
        (goto-char (point-min))
        (elxiki-line-fold)
        (while (setq pos (elxiki-line-find-sibling))
          (goto-char pos)
          (elxiki-line-fold))
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
            (setq item-function (elxiki-menu-get-action menu "root"))
            (if item-function
                (setq result (funcall item-function context))
              (setq result
                    (with-current-buffer (elxiki-menu-get-buffer menu)
                      (buffer-substring-no-properties (point-min) (point-max))))))
           ;; There is a function defined for this action, so do it.
           (item-function
            (setq result (funcall item-function context)))
           ;; No function, so just read from buffer.
           ('else
            (with-current-buffer (elxiki-menu-get-buffer menu)
              (goto-char (point-min))
              (goto-char
               (elxiki-line-follow-route
                (split-string inner-path "/" 'noempty)))
              (let ((children (elxiki-line-find-all-children)))
                (when children
                  (setq result
                        (apply 'buffer-substring-no-properties children)))))))
          (elxiki-menu/prepare result))
      (when menu
        (elxiki-menu-dispose menu)))))

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
