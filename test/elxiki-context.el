(require 'elxiki-context)
(require 'ert)

(ert-deftest elxiki-context-from-ancestry/menu-single ()
  "`elxiki-context-from-ancestry' reading a single item menu."
  (should
   (equal (elxiki-context-from-ancestry
           '(("@ " "menu")))
          '("@ " "menu" nil "menu" menu))))

(ert-deftest elxiki-context-from-ancestry/directory-single ()
  "`elxiki-context-from-ancestry' reading a single item directory."
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should
     (equal (elxiki-context-from-ancestry
             '(("+ " "~")))
            `("+ " "~" ,home nil directory)))))

(ert-deftest elxiki-context-from-ancestry/menu-no-prefix ()
  "`elxiki-context-from-ancestry' reading a menu item with no prefix."
  (should
   (equal (elxiki-context-from-ancestry
           '((nil "menu")))
          '(nil "menu" nil "menu" menu))))

(ert-deftest elxiki-context-from-ancestry/directory-no-prefix ()
  "`elxiki-context-from-ancestry' reading a directory with no prefix."
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should
     (equal (elxiki-context-from-ancestry
             '((nil "~")))
            `(nil "~" ,home nil directory)))))

(ert-deftest elxiki-context-from-ancestry/directory-and-menus ()
  "`elxiki-context-from-ancestry' reading a directory and a menu."
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should
     (equal (elxiki-context-from-ancestry
             '(("+ " "~")
               ("@ " "menu")))
            `("@ " "menu" ,home "menu" menu)))))

(ert-deftest elxiki-context-from-ancestry/directory-multiple ()
  "`elxiki-context-from-ancestry' reading a multi-level directory."
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should
     (equal (elxiki-context-from-ancestry
             '(("+ " "~")
               ("- " "sub")))
            `("- " "sub" ,(concat home "sub/") nil directory)))))

(ert-deftest elxiki-context-from-ancestry/menu-dir-menu ()
  "`elxiki-context-from-ancestry' reading a menu, directory, menu."
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should
     (equal (elxiki-context-from-ancestry
             '(("- " "menu1")
               ("+ " "~")
               ("@ " "menu2")))
            `("@ " "menu2" ,home "menu2" menu)))))

(ert-deftest elxiki-context-from-ancestry/other-prefix ()
  "`elxiki-context-from-ancestry' reading a weird prefix."
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should
     (equal (elxiki-context-from-ancestry
             '(("- " "~")
               ("! " "emacs")))
            `("! " "emacs" ,home nil misc)))))

(ert-deftest elxiki-context-create ()
  "`elxiki-context-create': various."
  (let ((context (elxiki-context-create)))
    (should-not (elxiki-context-get-prefix context))
    (should-not (elxiki-context-get-name context))
    (should (string-equal default-directory
                          (elxiki-context-get-directory context)))
    (should-not (elxiki-context-get-menu context))
    (should-not (elxiki-context-get-type context)))
  (let ((context (elxiki-context-create
                  :name 'name
                  :prefix 'prefix
                  :directory 'directory
                  :type 'type
                  :menu 'menu)))
    (should (equal 'prefix (elxiki-context-get-prefix context)))
    (should (equal 'name (elxiki-context-get-name context)))
    (should (equal 'directory (elxiki-context-get-directory context)))
    (should (equal 'menu (elxiki-context-get-menu context)))
    (should (equal 'type (elxiki-context-get-type context)))))
