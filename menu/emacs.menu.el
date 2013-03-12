(defmenu _root
  (format "
+ version/
& %s
@ emacs wiki/
"
user-emacs-directory))

(defmenu version
  (with-temp-buffer
    (pprint (emacs-version) (current-buffer) 50)
    (elxiki-prefix-buffer "| ")
    (buffer-string)))
