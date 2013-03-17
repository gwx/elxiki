;;; elxiki-font.el --- Font Lock for elxiki minor mode.

;;; Code:
(defvar elxiki-use-font-lock t
  "If nil, elxiki will not modify fonts when turned on.")

(defvar elxiki-mode-keywords nil
  "List of font lock keywords to use for elxiki mode.
Has the same format as `font-lock-keywords'. Use
`elxiki-font-define-prefix-face' to create entries.")

(defmacro elxiki-font-define-prefix-face (name prefix prefix-face name-face)
  "Define an elxiki prefix face.
NAME is a string naming the faces. PREFIX is the prefix
associated with the faces. PREFIX-FACE and NAME-FACE are the two
faces used for the prefix and name of the given elxiki lines."
  (declare (indent 2))
  (let* ((base-name (symbol-name name))
         (prefix-face-name
          (intern (concat "elxiki-" base-name "-prefix-face")))
         (name-face-name
          (intern (concat "elxiki-" base-name "-name-face")))
         (regexp-name
          (intern (concat "elxiki-" base-name "-regexp"))))
  `(progn
     (defface ,prefix-face-name ,prefix-face
       ,(concat "Face for the " base-name " prefix ( " prefix " )."))
     (defface ,name-face-name ,name-face
       ,(concat "Face for the " base-name " prefix ( " prefix " ) name."))
     (defvar ,regexp-name
       (rx line-start (* blank) (group ,prefix) (group (* nonl)))
       ,(concat "Regular expression for the elxiki " base-name " face."))
     (add-to-list 'elxiki-mode-keywords 
                  (list ,regexp-name 1 '',prefix-face-name t))
     (add-to-list 'elxiki-mode-keywords 
                  (list ,regexp-name 2 '',name-face-name t)))))

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(" (group word-start "elxiki-font-define-prefix-face" word-end))
    . 1)))

(elxiki-font-define-prefix-face menu "@ "
  '((t :inherit (variable-pitch font-lock-function-name-face) :height 1.3))
  '((t :inherit (variable-pitch font-lock-string-face) :height 1.2)))

(elxiki-font-define-prefix-face closed "+ "
  '((t :inherit (variable-pitch font-lock-function-name-face) :height 1.3))
  '((t :inherit (variable-pitch font-lock-string-face) :height 1.2)))

(elxiki-font-define-prefix-face opened "- "
  '((t :inherit (variable-pitch font-lock-function-name-face) :height 1.3))
  '((t :inherit (variable-pitch font-lock-string-face) :height 1.2)))

(elxiki-font-define-prefix-face text "|"
  '((t :inherit 'shadow))
  '((t :inherit 'font-lock-comment-face)))

(elxiki-font-define-prefix-face heading "> "
  '((t :inherit (variable-pitch font-lock-function-name-face) :height 1.4))
  '((t :inherit (variable-pitch font-lock-string-face) :height 1.4)))

(elxiki-font-define-prefix-face emacs-lisp "! "
  '((t :inherit 'font-lock-function-name-face :height 1.3))
  '((t :inherit 'font-lock-string-face :height 1.2)))

(elxiki-font-define-prefix-face emacs-lisp-silent "!! "
  '((t :inherit 'font-lock-function-name-face :height 1.3))
  '((t :inherit 'font-lock-string-face :height 1.2)))

(elxiki-font-define-prefix-face find "& "
  '((t :inherit (variable-pitch shadow) :height 1.2))
  '((t :inherit 'variable-pitch :height 1.2)))

(elxiki-font-define-prefix-face shell-sync "$ "
  '((t :inherit 'font-lock-builtin-face :height 1.3))
  '((t :inherit 'font-lock-variable-face :height 1.2)))

(elxiki-font-define-prefix-face shell-async "% "
  '((t :inherit 'font-lock-builtin-face :height 1.3))
  '((t :inherit 'font-lock-variable-face :height 1.2)))

(provide 'elxiki-font)
;;; elxiki-font.el ends here
