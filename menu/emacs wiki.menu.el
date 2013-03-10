(defmenu _root "
| http://www.emacswiki.org/
+ search/
")

(defmenu search "
  | Enter a search term as an sub-menu.
  | i.e. @ emacs wiki/search/faces
")

(defmenu _undefined
  (let ((term (elxiki-drop-prefix "emacs wiki/search/" menu)))
    (when term
      (browse-url 
       (concat "http://www.google.com/cse"
               "?cx=004774160799092323420%3A6-ff2s0o6yi&q="
               term)))))
