(require 'elxiki-menu)
(require 'ert)
(require 'cl-lib)

(ert-deftest elxiki-menu-find/shadow-1 ()
  "`elxiki-menu-find': earlier menus in `elxiki-menu-path' shadow
later ones."
  (let ((elxiki-menu-path '("/a/" "/b/"))
        (menu-files '("/a/1.menu" "/b/1.menu" "/b/2.menu")))
    (cl-letf (((symbol-function 'file-exists-p)
               #'(lambda (filename) (member filename menu-files))))
      (should (string-equal "/a/1.menu"
                            (elxiki-menu-find "1")))
      (should (string-equal "/b/2.menu"
                            (elxiki-menu-find "2"))))))

(ert-deftest elxiki-menu-find/shadow-2 ()
  "`elxiki-menu-find': simple menus should shadow complex ones."
  (let ((elxiki-menu-path '("/a/"))
        (menu-files '("/a/1.menu" "/a/1.menu.el" "/a/2.menu.el")))
    (cl-letf (((symbol-function 'file-exists-p)
               #'(lambda (filename) (member filename menu-files))))
      (should (string-equal "/a/1.menu"
                            (elxiki-menu-find "1")))
      (should (string-equal "/a/2.menu.el"
                            (elxiki-menu-find "2"))))))

(ert-deftest elxiki-menu-load/nonexistant ()
  "`elxiki-menu-load': loading a nonexistant menu."
  (should-not (elxiki-menu-load nil)))

(ert-deftest elxiki-menu-load/force ()
  "`elxiki-menu-load': force loading a nonexistant menu."
  (let* (;; Don't find an existing menu by accident.
         (elxiki-menu-path nil)
         ;; Place the menu in the root directory.
         (elxiki-menu-directory "/")
         (context (elxiki-context-create :menu "a/b/c"))
         (menu (elxiki-menu-load context 'force)))
    (unwind-protect
        (with-current-buffer (elxiki-menu-get-buffer menu)
          (should (string-equal (buffer-string) ""))
          (should-not (elxiki-menu-get-actions menu))
          (should (string-equal "/a.menu" (elxiki-menu-get-file menu))))
      (elxiki-menu-dispose menu))))

(ert-deftest elxiki-menu-load/basic ()
  "`elxiki-menu-load': loading a basic menu."
  (let ((menu-file (make-temp-file "ert-elxiki-menu-" nil ".menu"))
        (menu-def "+ A\n+ B\n+ C")
        menu)
    (with-temp-file menu-file
      (insert menu-def))
    (cl-letf (;; Always find the temp file as a menu regardless of context.
              ((symbol-function 'elxiki-menu-find)
               #'(lambda (arg) menu-file)))
      (setq menu (elxiki-menu-load nil))
      (unwind-protect
          (with-current-buffer (elxiki-menu-get-buffer menu)
            (should (string-equal menu-file
                                  (elxiki-menu-get-file menu)))
            (should (string-equal (buffer-string) menu-def)))
        (elxiki-menu-dispose menu)))))

;; menu-load advanced.
