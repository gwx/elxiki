;;; elxiki.el --- A pale mimicry of Xiki implemented in elisp.

;;; Commentary:
;; This defines the minor modes `elxiki-mode' and
;; `elxiki-global-mode', which attempts to mimic some of the features
;; of Xiki (xiki.org), but without using all those ruby libraries.
;; 
;; See the accompanying readme.

;;; Code:
(defvar elxiki-directory (file-name-directory (or (buffer-file-name) load-file-name))
  "The home directory for elxiki.")
(defvar elxiki-test-directory 
  (file-name-directory (concat elxiki-directory "test/"))
  "The directory for elxiki tests.")
(add-to-list 'load-path (concat elxiki-directory "src/"))

(require 'elxiki-util)
(require 'elxiki-line)
(require 'elxiki-context)
(require 'elxiki-menu)
(require 'elxiki-filter)
(require 'elxiki-command)
(require 'elxiki-font)
(require 'elxiki-interactive)

(defvar elxiki-mode-map (make-sparse-keymap)
  "Keymap for ElXiKi mode.")

(define-key elxiki-mode-map (kbd "C-<return>") 'elxiki-command-no-filter)
(define-key elxiki-mode-map (kbd "M-<return>") 'elxiki-command)
(define-key elxiki-mode-map (kbd "C-c [") 'elxiki-menu-edit)

(define-minor-mode elxiki-mode "Elxiki mode."
  :lighter " ElXiKi"
  :keymap elxiki-mode-map
  (if (and elxiki-mode elxiki-use-font-lock)
      (font-lock-add-keywords nil elxiki-mode-keywords)
    (font-lock-remove-keywords nil elxiki-mode-keywords))
  (font-lock-fontify-buffer))

(defun elxiki-load-tests ()
  "Load the elxiki tests so you can run them with `ert'."
  (interactive)
  (mapc 'load (directory-files elxiki-test-directory
                               t
                               (rx ".el" string-end)
                               'no-sort)))

(provide 'elxiki)
;;; elxiki.el ends here
