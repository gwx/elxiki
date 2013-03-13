;;; elxiki-font.el --- Font Lock for elxiki minor mode.

;;; Code:
(defvar elxiki-use-font-lock t
  "If nil, elxiki will not modify fonts when turned on.")

(defface elxiki-closed-prefix-face
  '((t :inherit (variable-pitch font-lock-function-name-face) :height 1.3))
  "Face for the closed prefix ( + ).")

(defface elxiki-closed-name-face
  '((t :inherit (variable-pitch font-lock-string-face) :height 1.15))
  "Face for the closed prefix ( + ) names.")

(defface elxiki-opened-prefix-face
  '((t :inherit (variable-pitch font-lock-function-name-face) :height 1.3))
  "Face for the opened prefix ( - ).")

(defface elxiki-opened-name-face
  '((t :inherit (variable-pitch font-lock-string-face) :height 1.15))
  "Face for the opened prefix ( - ) names.")

(defface elxiki-point-prefix-face
  '((t :inherit (variable-pitch shadow) :height 1.3))
  "Face for the point prefix ( & ).")

(defface elxiki-point-name-face
  '((t :inherit 'variable-pitch :height 1.15))
  "Face for the point prefix ( & ) names.")

(defface elxiki-output-prefix-face
  '((t :inherit 'shadow))
  "Face for the output prefix ( | ).")

(defface elxiki-output-name-face
  '((t :inherit 'font-lock-comment-face))
  "Face for the output prefix ( | ) names.")

(defface elxiki-shell-prefix-face
  '((t :inherit 'font-lock-builtin-face))
  "Face for the shell prefix ( $ )..")

(defface elxiki-shell-name-face
  '((t :inherit 'font-lock-function-name-face))
  "Face for the shell prefix ( | ) names.")

(defface elxiki-async-prefix-face
  '((t :inherit 'font-lock-builtin-face))
  "Face for the async prefix ( % ).")

(defface elxiki-async-name-face
  '((t :inherit 'font-lock-function-name-face))
  "Face for the async prefix ( % ) names.")

(defface elxiki-code-prefix-face
  '((t :inherit 'font-lock-function-name-face))
  "Face for the code prefix ( ! ).")

(defface elxiki-code-name-face
  '((t :inherit 'font-lock-builtin-face))
  "Face for the code prefix ( ! ) names.")

(defface elxiki-code-silent-prefix-face
  '((t :inherit 'font-lock-function-name-face))
  "Face for the silent code prefix ( !! ).")

(defface elxiki-code-silent-name-face
  '((t :inherit 'font-lock-builtin-face))
  "Face for the silent code prefix ( !! ) names.")

(defface elxiki-menu-prefix-face
  '((t :inherit 'font-lock-function-name-face))
  "Face for the menu prefix ( @ ).")

(defface elxiki-menu-name-face
  '((t :inherit (variable-pitch font-lock-string-face) :height 1.15))
  "Face for the menu prefix ( @ ) names.")

(defface elxiki-heading-prefix-face
  '((t :inherit (variable-pitch font-lock-builtin-face) :height 1.3))
  "Face for the heading prefix ( > ).")

(defface elxiki-heading-name-face
  '((t :inherit (variable-pitch font-lock-function-name-face) :height 1.3))
  "Face for the heading prefix ( > ) names.")

;;; Keyword Regexs
(defvar elxiki-closed-regex
  (rx line-start (* blank) (group "+ ") (group (* nonl)))
  "Regular expression for the elxiki closed face.")

(defvar elxiki-opened-regex
  (rx line-start (* blank) (group "- ") (group (* nonl)))
  "Regular expression for the elxiki opened face.")

(defvar elxiki-point-regex
  (rx line-start (* blank) (group "& ") (group (* nonl)))
  "Regular expression for the elxiki point face.")

(defvar elxiki-output-regex
  (rx line-start (* blank) (group "|" (? " ")) (group (* nonl)))
  "Regular expression for elxiki output.")

(defvar elxiki-shell-regex
  (rx line-start (* blank) (group "$ ") (group (* nonl)))
  "Regex for highlighting shell commands.")

(defvar elxiki-async-regex
  (rx line-start (* blank) (group "% ") (group (* nonl)))
  "Regex for highlighting async commands.")

(defvar elxiki-code-regex
  (rx line-start (* blank) (group "! ") (group (* nonl)))
  "Regular expression for the elxiki code face.")

(defvar elxiki-code-silent-regex
  (rx line-start (* blank) (group "!! ") (group (* nonl)))
  "Regular expression for the elxiki code face.")

(defvar elxiki-menu-regex
  (rx line-start (* blank) (group "@ ") (group (* nonl)))
  "Regular expression for the elxiki menu face.")

(defvar elxiki-heading-regex
  (rx line-start (* blank) (group "> ") (group (* nonl)))
  "Regular expression for the elxiki heading face.")

;;; Keyword Definition
(defvar elxiki-mode-keywords
  `((,elxiki-closed-regex 1 'elxiki-closed-prefix-face t)
    (,elxiki-closed-regex 2 'elxiki-closed-name-face t)
    (,elxiki-opened-regex 1 'elxiki-opened-prefix-face t)
    (,elxiki-opened-regex 2 'elxiki-opened-name-face t)
    (,elxiki-point-regex 1 'elxiki-point-prefix-face t)
    (,elxiki-point-regex 2 'elxiki-point-name-face t)
    (,elxiki-output-regex 1 'elxiki-output-prefix-face t)
    (,elxiki-output-regex 2 'elxiki-output-name-face t)
    (,elxiki-shell-regex 1 'elxiki-shell-prefix-face t)
    (,elxiki-shell-regex 2 'elxiki-shell-name-face t)
    (,elxiki-async-regex 1 'elxiki-async-prefix-face t)
    (,elxiki-async-regex 2 'elxiki-async-name-face t)
    (,elxiki-code-regex 1 'elxiki-code-prefix-face t)
    (,elxiki-code-regex 2 'elxiki-code-name-face t)
    (,elxiki-code-silent-regex 1 'elxiki-code-silent-prefix-face t)
    (,elxiki-code-silent-regex 2 'elxiki-code-silent-name-face t)
    (,elxiki-menu-regex 1 'elxiki-menu-prefix-face t)
    (,elxiki-menu-regex 2 'elxiki-menu-name-face t)
    (,elxiki-heading-regex 1 'elxiki-heading-prefix-face t)
    (,elxiki-heading-regex 2 'elxiki-heading-name-face t))
  "List to pass to `font-lock-add-keywords'.")

(provide 'elxiki-font)
;;; elxiki-font.el ends here
