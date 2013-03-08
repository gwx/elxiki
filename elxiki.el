;;; elxiki.el --- A pale mimicry of Xiki implemented in elisp.

;;; Commentary:
;; This defines the minor modes `elxiki-mode' and
;; `elxiki-global-mode', which attempts to mimic some of the features
;; of Xiki (xiki.org), but without using all those ruby libraries.
;; 
;; See the accompanying readme.

;;; Code:
(defvar elxiki-dir (file-name-directory (or (buffer-file-name) load-file-name))
  "The home directory for elxiki.")
(add-to-list 'load-path elxiki-dir)

(require 'elxiki-util)
(require 'elxiki-line)
(require 'elxiki-context)
(require 'elxiki-command)
(require 'elxiki-font)
(require 'elxiki-interactive)

(defvar elxiki-mode-map (make-sparse-keymap)
  "Keymap for ElXiKi mode.")

(define-key elxiki-mode-map (kbd "C-<return>") 'elxiki-command)
(define-key elxiki-mode-map (kbd "M-<return>") 'elxiki-command)

(define-minor-mode elxiki-mode nil
  :lighter " ElXiKi"
  :keymap elxiki-mode-map
  (if (and elxiki-mode elxiki-use-font-lock)
      (font-lock-add-keywords nil elxiki-mode-keywords 'low-precedence)
    (font-lock-remove-keywords nil elxiki-mode-keywords))
  (font-lock-fontify-buffer))

;; Disables font-lock by default.
(define-globalized-minor-mode elxiki-global-mode elxiki-mode
  (lambda ()
    (unless elxiki-mode
      (let ((elxiki-use-font-lock nil))
        (elxiki-mode 1)))))

(provide 'elxiki)
;;; elxiki.el ends here
