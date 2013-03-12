(defmenu _init
  (concat 
   "> Activate a line to set it.
| e.g. @ env/aaa=bbb\n"
   (mapconcat (lambda (s) (concat "- " s))
             process-environment
             "\n")))

(defmenu _root/_many
  (let ((var (elxiki-drop-root menu)))
    (unless (= ?> (string-to-char var))
      (setq var (split-string var "="))
      (setenv (car var)
              (or (cadr var) ""))))
  "")
