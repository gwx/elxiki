;;; elxiki-filter.el --- Filter newly opened folders.

(require 'elxiki-line)

(defvar elxiki-filter-function 'elxiki-filter-function-default
  "Function to use when filtering elxiki lines.
Takes a filter string and returns a predicate to be run when
point is on a given line.")

(defun elxiki-filter-function-default (input)
  "Default filter function for elxiki."
  (setq *elxiki-filter-regexp*
        (concat (rx (* nonl))
                (mapconcat 'regexp-quote
                           (split-string input "" 'no-blanks)
                           (rx (* nonl)))
                (rx (* nonl))))
  (lambda ()
    (save-excursion
      (elxiki-line-goto-name)
      (looking-at *elxiki-filter-regexp*))))

(defvar elxiki-filter-map (make-keymap)
  "Keymap while elxiki is filtering results.")

(defvar elxiki-filter-input nil
  "Current input to the `elxiki-filter-function'.")

(defun elxiki-filter ()
  "Start filtering the elxiki children at point."
  (interactive)
  (setq elxiki-filter-input "")
  (when overriding-local-map
    (error "Something is already using `overriding-local-map'."))
  (setq overriding-local-map elxiki-filter-map))

(defun elxiki-filter-stop ()
  "Stop filtering elxiki results."
  (interactive)
  (message "Stopped filtering.")
  (setq overriding-local-map nil))

(defun elxiki-filter-insert ()
  "Inserts the key pressed into the filter list."
  (interactive)
  (let ((keys (this-command-keys)))
    (setq elxiki-filter-input
          (concat elxiki-filter-input
                  (substring keys (- (length keys) 1))))
    (elxiki-line-filter-children
     (funcall elxiki-filter-function elxiki-filter-input)
     t)))

(define-key elxiki-filter-map [t] 'elxiki-filter-stop)
(substitute-key-definition 'self-insert-command 'elxiki-filter-insert
                           elxiki-filter-map global-map)
(define-key elxiki-filter-map (kbd "C-g") 'elxiki-filter-stop)
(define-key elxiki-filter-map (kbd "RET") 'elxiki-filter-stop)

(provide 'elxiki-filter)

;;; elxiki-filter.el ends here
