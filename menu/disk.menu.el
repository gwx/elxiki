(defmenu _root
  (mapconcat
   (lambda (s) (concat "| " s))
   (split-string (shell-command-to-string "df -h") "\n" 'strip-empty)
   "\n"))
