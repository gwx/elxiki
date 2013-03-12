(defmenu _root
  (concat "> Choose a theme:\n"
          (mapconcat (lambda (theme) (concat "- " (symbol-name theme)))
                     (custom-available-themes)
                     "\n")))

(defmenu _root/_many
  (load-theme (intern name))
  "")
