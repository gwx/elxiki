(defmenu _root "
| You can enter a search term as an sub-menu.
| i.e. @ google image/cats
")

(defmenu _root/_many
  (let ((term (elxiki-drop-root menu)))
    (when term
      (browse-url
       (concat "http://www.google.com/images?q="
               term)))
    ""))
