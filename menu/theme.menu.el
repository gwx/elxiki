(defmenu _root
  (concat "> Choose a theme:\n"
          (mapconcat (lambda (theme) (concat "- " (symbol-name theme)))
                     (custom-available-themes)
                     "\n")))

(defmenu _undefined
  (load-theme (intern name))
  "")
