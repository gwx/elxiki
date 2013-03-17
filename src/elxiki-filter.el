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

(defvar elxiki-filter-inhibit nil
  "When non-nil, ignore calls to `elxiki-filter'.")

(defvar elxiki-filter-map (make-keymap)
  "Keymap while elxiki is filtering results.")

(defvar elxiki-filter-input nil
  "Current input to the `elxiki-filter-function'.")

(defvar elxiki-filter-cursor-type 'hollow
  "Cursor type to change cursor to while filtering results.
If nil, do not change cursor.")

(defvar elxiki-filter-cursor-previous nil
  "Holds the previous cursor type.")

(define-minor-mode elxiki-filter
  "Currently filtering elxiki results.
See `elxiki-filter-map'."
  :keymap 'elxiki-filter-map
  (if elxiki-filter
      (if (and (not elxiki-filter-inhibit)
               (elxiki-line-goto-first-child))
          (progn
            (setq elxiki-filter-input "")
            (when elxiki-filter-cursor-type
              (setq elxiki-filter-cursor-previous cursor-type)
              (setq cursor-type elxiki-filter-cursor-type)))
        (elxiki-filter -1))
    (when (and elxiki-filter-cursor-type
               elxiki-filter-cursor-previous)
      (setq cursor-type elxiki-filter-cursor-previous))))

(defun elxiki-filter-stop ()
  "Stop filtering elxiki results."
  (interactive)
  (elxiki-filter -1))

(defun elxiki-filter-insert ()
  "Inserts the key pressed into the filter list."
  (interactive)
  (let ((keys (this-command-keys)))
    (setq elxiki-filter-input
          (concat elxiki-filter-input
                  (substring keys (- (length keys) 1))))
    (elxiki-line-filter-siblings
     (funcall elxiki-filter-function elxiki-filter-input)
     t)))

(defun elxiki-filter-replace-parent ()
  "Make the entry replace the parent and then act on it."
  (interactive)
  (when (save-excursion (elxiki-line-goto-parent))
    (elxiki-line-replace-parent)
    (elxiki-filter -1)
    (elxiki-command)))

(defun elxiki-filter-hide-siblings ()
  "Make the line at point hide siblings and then act on it."
  (interactive)
  (elxiki-line-delete-siblings)
  (elxiki-filter -1)
  (elxiki-command))

(define-key elxiki-filter-map [t] 'elxiki-filter-stop)
(substitute-key-definition 'self-insert-command 'elxiki-filter-insert
                           elxiki-filter-map global-map)
(define-key elxiki-filter-map (kbd "C-g") 'elxiki-filter-stop)
(define-key elxiki-filter-map (kbd "SPC") 'elxiki-filter-stop)
(define-key elxiki-filter-map (kbd "C-h") nil)
(define-key elxiki-filter-map (kbd "<return>") 'elxiki-command)
(define-key elxiki-filter-map (kbd "C-<return>") 'elxiki-command-no-filter)
(define-key elxiki-filter-map (kbd "M-<return>") 'elxiki-command)
(define-key elxiki-filter-map (kbd "<tab>") 'elxiki-filter-hide-siblings)
(define-key elxiki-filter-map (kbd "C-/") 'elxiki-filter-replace-parent)
(define-key elxiki-filter-map (kbd "M-/") 'elxiki-filter-replace-parent)

(provide 'elxiki-filter)

;;; elxiki-filter.el ends here
