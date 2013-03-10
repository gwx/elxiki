(defmenu root
  (let ((menus (sort (elxiki-menu-all) 'string-lessp))
        (text ""))
    (mapconcat (lambda (line) (concat "@ " line))
               menus
               "\n")))
