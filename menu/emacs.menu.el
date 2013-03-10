(defmenu _init "
+ version/
@ emacs wiki/
")

(defmenu version
  (with-temp-buffer
    (pprint (emacs-version) (current-buffer) 50)
    (elxiki-prefix-buffer "| ")
    (buffer-string)))
