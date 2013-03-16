(defmenu _root "| You can enter a search term as an sub-menu.
| i.e. @ google/emacs
| Same for the below:
@ google image/")

(defmenu _root/_many
  (let ((term (elxiki-drop-root menu)))
    (when term
      (browse-url
       (concat "http://www.google.com/search?q="
               term
               "&ie=utf-8&oe=utf-8&aq=t")))))
