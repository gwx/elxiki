;;; elxiki-font.el --- Font Lock for elxiki minor mode.

;;; Code:
(defvar elxiki-use-font-lock t
  "If nil, elxiki will not modify fonts when turned on.")

(defface elxiki-closed-prefix-face
  '((t :inherit 'font-lock-function-name-face))
  "Face for the closed prefix ( + ).")

(defface elxiki-closed-name-face
  '((t :inherit 'font-lock-string-face))
  "Face for the closed prefix ( + ) names.")

(defface elxiki-opened-prefix-face
  '((t :inherit 'shadow))
  "Face for the opened prefix ( - ).")

(defface elxiki-opened-name-face
  '((t :inherit 'font-lock-string-face))
  "Face for the opened prefix ( - ) names.")

(defface elxiki-point-prefix-face
  '((t :inherit 'shadow))
  "Face for the point prefix ( * ).")

(defface elxiki-point-name-face
  '((t :inherit 'default))
  "Face for the point prefix ( * ) names.")

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

;;; Keyword Regexs
(defvar elxiki-closed-regex
  (rx line-start (* blank) (group "+ ") (group (* nonl)))
  "Regular expression for the elxiki closed face.")

(defvar elxiki-opened-regex
  (rx line-start (* blank) (group "- ") (group (* nonl)))
  "Regular expression for the elxiki opened face.")

(defvar elxiki-point-regex
  (rx line-start (* blank) (group "* ") (group (* nonl)))
  "Regular expression for the elxiki point face.")

(defvar elxiki-output-regex
  (rx line-start (* blank) (group "| ") (group (* nonl)))
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

;;; Keyword Definition
(defvar elxiki-mode-keywords
  `((,elxiki-closed-regex 1 'elxiki-closed-prefix-face)
    (,elxiki-closed-regex 2 'elxiki-closed-name-face)
    (,elxiki-opened-regex 1 'elxiki-opened-prefix-face)
    (,elxiki-opened-regex 2 'elxiki-opened-name-face)
    (,elxiki-point-regex 1 'elxiki-point-prefix-face)
    (,elxiki-point-regex 2 'elxiki-point-name-face)
    (,elxiki-output-regex 1 'elxiki-output-prefix-face)
    (,elxiki-output-regex 2 'elxiki-output-name-face)
    (,elxiki-shell-regex 1 'elxiki-shell-prefix-face)
    (,elxiki-shell-regex 2 'elxiki-shell-name-face)
    (,elxiki-async-regex 1 'elxiki-async-prefix-face)
    (,elxiki-async-regex 2 'elxiki-async-name-face)
    (,elxiki-code-regex 1 'elxiki-code-prefix-face)
    (,elxiki-code-regex 2 'elxiki-code-name-face))
  "List to pass to `font-lock-add-keywords'.")

(provide 'elxiki-font)
;;; elxiki-font.el ends here
