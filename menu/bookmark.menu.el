(defmenu _init
  (mapconcat (lambda (line) (concat "- " line))
             (bookmark-all-names)
             "\n"))

(defmenu _root/_many
  (let ((bookmark (elxiki-drop-root menu)))
    (when bookmark
      (bookmark-jump bookmark))))
