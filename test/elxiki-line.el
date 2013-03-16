(require 'elxiki-line)
(require 'ert)

(defvar elxiki-line-test--string-1 "
+ abc
+ def
  ! egh

- ijk
  % aou
    & aii
menu/no/prefix/

Here's some normal text to throw you off
")

(ert-deftest elxiki-line-goto-beginning ()
  "`elxiki-line-goto-beginning' moves to beginning of line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 16)
    (let ((start-line (line-number-at-pos)))
      (elxiki-line-goto-beginning)
      (should (bolp))
      (should (= start-line (line-number-at-pos))))))

(ert-deftest elxiki-line-blank-p/valid ()
  "`elxiki-line-blank-p' on a blank line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 22)
    (should (elxiki-line-blank-p))))

(ert-deftest elxiki-line-blank-p/valid-end ()
  "`elxiki-line-blank-p' on the end of a blank line."
  (with-temp-buffer
    (insert "act
now
   
to")
    (goto-char 12)
    (should (elxiki-line-blank-p))))

(ert-deftest elxiki-line-blank-p/nil ()
  "`elxiki-line-blank-p' on a normal line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 17)
    (should-not (elxiki-line-blank-p))))

(ert-deftest elxiki-line-goto-prefix/no-indent ()
  "`elxiki-line-goto-prefix' with no indentation."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 3)
    (let ((start-line (line-number-at-pos)))
      (elxiki-line-goto-prefix)
      (should (bolp))
      (should (= start-line (line-number-at-pos))))))

(ert-deftest elxiki-line-goto-prefix/indent ()
  "`elxiki-line-goto-prefix' with some indentation."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 18)
    (let ((start-line (line-number-at-pos)))
      (elxiki-line-goto-prefix)
      (should-not (bolp))
      (should (looking-at-p "!"))
      (should (= start-line (line-number-at-pos))))))

(ert-deftest elxiki-line-looking-at-prefix/succeed ()
  "`elxiki-line-looking-at-prefix' on a + line."
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "+ ")
    (goto-char 0)
    (should (string-equal "+ " (elxiki-line-looking-at-prefix)))))

(ert-deftest elxiki-line-looking-at-prefix/fail ()
  "`elxiki-line-looking-at-prefix' on a * line."
  (with-temp-buffer
    (display-buffer (current-buffer))
    (insert "* ")
    (goto-char 0)
    (should-not (elxiki-line-looking-at-prefix))))

(ert-deftest elxiki-line-goto-name/prefix ()
  "`elxiki-line-goto-prefix' with a prefix."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 15)
    (let ((start-line (line-number-at-pos)))
      (elxiki-line-goto-name)
      (should-not (bolp))
      (should (looking-at-p "egh"))
      (should (= start-line (line-number-at-pos))))))

(ert-deftest elxiki-line-goto-name/no-prefix ()
  "`elxiki-line-goto-prefix' with no prefix."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 49)
    (let ((start-line (line-number-at-pos)))
      (elxiki-line-goto-name)
      (should (bolp))
      (should (looking-at-p "menu/"))
      (should (= start-line (line-number-at-pos))))))

(ert-deftest elxiki-line-get-prefix/succeed ()
  "`elxiki-line-get-prefix' on a ! line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 17)
    (should (string-equal "! " (elxiki-line-get-prefix)))
    (should (= 17 (point)))))

(ert-deftest elxiki-line-get-prefix/fail ()
  "`elxiki-line-get-prefix' on a non-elxiki line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 60)
    (should-not (elxiki-line-get-prefix))
    (should (= 60 (point)))))

(ert-deftest elxiki-line-get-name/no-indent ()
  "`elxiki-line-get-name' on a + line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 11)
    (should (string-equal "def" (elxiki-line-get-name)))
    (should (= 11 (point)))))

(ert-deftest elxiki-line-get-name/indent ()
  "`elxiki-line-get-name' on a menu/ line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 51)
    (should (string-equal "menu/no/prefix/" (elxiki-line-get-name)))
    (should (= 51 (point)))))

(ert-deftest elxiki-line-valid-p/valid ()
  "`elxiki-line-valid-p' on a + line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 11)
    (should (elxiki-line-valid-p))
    (should (= 11 (point)))))

(ert-deftest elxiki-line-valid-p/valid-2 ()
  "`elxiki-line-valid-p' on a menu/ line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 51)
    (should (elxiki-line-valid-p))
    (should (= 51 (point)))))

(ert-deftest elxiki-line-valid-p/invalid ()
  "`elxiki-line-valid-p' on a menu/ line."
  (with-temp-buffer
    (insert elxiki-line-test--string-1)
    (goto-char 71)
    (should-not (elxiki-line-valid-p))
    (should (= 71 (point)))))

(ert-deftest elxiki-line-goto-current/same ()
  "`elxiki-line-goto-current' on a elxiki line."
  (with-temp-buffer
    (insert "natoes\n+ Line\nasnetu")
    (goto-char 11)
    (should (elxiki-line-goto-current))
    (should (bolp))
    (should (= (line-number-at-pos) 2))
    (should (elxiki-line-valid-p))))

(ert-deftest elxiki-line-goto-current/blank-valid ()
  "`elxiki-line-goto-current' on a blank line after a valid one."
  (with-temp-buffer
    (insert "natoes
+ Line

 
asnetu")
    (goto-char 17)
    (should (elxiki-line-goto-current))
    (should (bolp))
    (should (= (line-number-at-pos) 2))
    (should (elxiki-line-valid-p))))

(ert-deftest elxiki-line-goto-current/same-invalid ()
  "`elxiki-line-goto-current' on an invalid line."
  (with-temp-buffer
    (insert "natoes
Turkesy
asnetu")
    (goto-char 11)
    (should-not (elxiki-line-goto-current))
    (should-not (bolp))
    (should-not (elxiki-line-valid-p))))

(ert-deftest elxiki-line-goto-current/blank-invalid ()
  "`elxiki-line-goto-current' on an invalid line."
  (with-temp-buffer
    (insert "natoes
Turkesy
  

  
asnetu")
    (goto-char 21)
    (should-not (elxiki-line-goto-current))
    (should-not (bolp))))


(ert-deftest elxiki-line-goto-next/valid ()
  "`elxiki-line-goto-next' on a valid setup."
  (with-temp-buffer
    (insert "act
+ Line 1
+ Line 2
asnetu")
    (goto-char 7)
    (should (elxiki-line-goto-next))
    (should (bolp))
    (should (= (point) 14))
    (should (elxiki-line-valid-p))))

(ert-deftest elxiki-line-goto-next/valid-blank ()
  "`elxiki-line-goto-next' on a valid setup with blanks."
  (with-temp-buffer
    (insert "act
+ Line 1

+ Line 2
asnetu")
    (goto-char 7)
    (should (elxiki-line-goto-next))
    (should (bolp))
    (should (= (point) 15))
    (should (elxiki-line-valid-p))))

(ert-deftest elxiki-line-goto-next/interrupt ()
  "`elxiki-line-goto-next' with an interrupting line."
  (with-temp-buffer
    (insert "act
+ Line 1

BAD LINE HERE
+ Line 2
asnetu")
    (goto-char 7)
    (should-not (elxiki-line-goto-next))
    (should (= (point) 7))
    (should (elxiki-line-valid-p))))

(ert-deftest elxiki-line-goto-next/buffer-end ()
  "`elxiki-line-goto-next' on the last line of the buffer."
  (with-temp-buffer
    (insert "act
+ Line 1")
    (goto-char 7)
    (should-not (elxiki-line-goto-next))
    (should (= (point) 7))
    (should (elxiki-line-valid-p))))

(ert-deftest elxiki-line-goto-next/invalid ()
  "`elxiki-line-goto-next' with an invalid line."
  (with-temp-buffer
    (insert "act
BAD LINE HERE
+ asnetu")
    (goto-char 7)
    (should-not (elxiki-line-goto-next))
    (should (= (point) 7))))

(ert-deftest elxiki-line-goto-previous/valid ()
  "`elxiki-line-goto-previous' on a valid setup."
  (with-temp-buffer
    (insert "act
+ Line 1
+ Line 2
asnetu")
    (goto-char 20)
    (should (elxiki-line-goto-previous))
    (should (bolp))
    (should (= (point) 5))))

(ert-deftest elxiki-line-goto-previous/valid-blank ()
  "`elxiki-line-goto-next' on a valid setup with blanks."
  (with-temp-buffer
    (insert "act
+ Line 1
  
+ Line 2
asnetu")
    (goto-char 20)
    (should (elxiki-line-goto-previous))
    (should (bolp))
    (should (= (point) 5))))

(ert-deftest elxiki-line-goto-previous/interrupt ()
  "`elxiki-line-goto-previous' with an interrupting line."
  (with-temp-buffer
    (insert "act
+ Line 1
BAD
 
+ Line 2
asnetu")
    (goto-char 25)
    (should-not (elxiki-line-goto-previous))
    (should (= (point) 25))))

(ert-deftest elxiki-line-goto-previous/buffer-start ()
  "`elxiki-line-goto-previous' on the first line of the buffer."
  (with-temp-buffer
    (insert "+ Line 1")
    (goto-char 3)
    (should-not (elxiki-line-goto-previous))
    (should (= (point) 3))))

(ert-deftest elxiki-line-goto-previous/invalid ()
  "`elxiki-line-goto-next' with an invalid line."
  (with-temp-buffer
    (insert "act
BAD LINE HERE
+ asnetu")
    (goto-char 7)
    (should-not (elxiki-line-goto-previous))
    (should (= (point) 7))))

(ert-deftest elxiki-line-goto-parent/direct ()
  "`elxiki-line-goto-parent' with the parent directly above."
  (with-temp-buffer
    (insert "+ Parent
  + Child")
    (goto-char 16)
    (should (elxiki-line-goto-parent))
    (should (= (point) 1))))

(ert-deftest elxiki-line-goto-parent/indirect ()
  "`elxiki-line-goto-parent' with the parent indirectly above."
  (with-temp-buffer
    (insert "+ P\n  + A\n    + S\n  \n  + C")
    (goto-char 25) ; C
    (should (= 1 (elxiki-line-goto-parent)))
    (should (= (point) 1))))

(ert-deftest elxiki-line-goto-parent/buffer-start ()
  "`elxiki-line-goto-parent' on the first line."
  (with-temp-buffer
    (insert " + Parent
   + Child")
    (goto-char 5)
    (should-not (elxiki-line-goto-parent))
    (should (= (point) 5))))

(ert-deftest elxiki-line-goto-parent/no-parent ()
  "`elxiki-line-goto-parent' without any parent."
  (with-temp-buffer
    (insert "+ A
+ B")
    (goto-char 6)
    (should-not (elxiki-line-goto-parent))
    (should (= (point) 6))))

(ert-deftest elxiki-line-goto-parent/interrupt ()
  "`elxiki-line-goto-parent' with an interruption."
  (with-temp-buffer
    (insert "+ P
BAD
  + Child")
    (goto-char 15)
    (should-not (elxiki-line-goto-parent))
    (should (= (point) 15))))

(ert-deftest elxiki-line-goto-parent/invalid ()
  "`elxiki-line-goto-parent' on an invalid line."
  (with-temp-buffer
    (insert "+ P
BAD LINE")
    (goto-char 8)
    (should-not (elxiki-line-goto-parent))
    (should (= (point) 8))))

(ert-deftest elxiki-line-goto-first-child/one ()
  "`elxiki-line-goto-first-child' with 1 child."
  (with-temp-buffer
    (insert "+ P
  + C")
    (goto-char 2)
    (should (= 5 (elxiki-line-goto-first-child)))
    (should (= 5 (point)))))

(ert-deftest elxiki-line-goto-first-child/many ()
  "`elxiki-line-goto-first-child' with many children."
  (with-temp-buffer
    (insert "+ P
  + C
  + E
  + G")
    (goto-char 2)
    (should (= 5 (elxiki-line-goto-first-child)))
    (should (= 5 (point)))))

(ert-deftest elxiki-line-goto-first-child/gap ()
  "`elxiki-line-goto-first-child' with a large gap before the child."
  (with-temp-buffer
    (insert "+ P
   
 
  + C")
    (goto-char 2)
    (should (= 11 (elxiki-line-goto-first-child)))
    (should (= 11 (point)))))

(ert-deftest elxiki-line-goto-first-child/none ()
  "`elxiki-line-goto-first-child' with no child."
  (with-temp-buffer
    (insert "+ P
+ S
  + C")
    (goto-char 2)
    (should-not (elxiki-line-goto-first-child))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-first-child/interrupt ()
  "`elxiki-line-goto-first-child' with an interruption."
  (with-temp-buffer
    (insert "+ P
BAD LINE
  + C")
    (goto-char 2)
    (should-not (elxiki-line-goto-first-child))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-first-child/invalid ()
  "`elxiki-line-goto-first-child' on an invalid line."
  (with-temp-buffer
    (insert "BAD LINE
  + S
    + C")
    (goto-char 2)
    (should-not (elxiki-line-goto-first-child))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-first-child/buffer-end ()
  "`elxiki-line-goto-first-child' at the end of the buffer."
  (with-temp-buffer
    (insert "+ P")
    (goto-char 2)
    (should-not (elxiki-line-goto-first-child))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-next-sibling/valid ()
  "`elxiki-line-goto-next-sibling' with a sibling."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 2)
    (should (= 5 (elxiki-line-goto-next-sibling)))
    (should (= 5 (point)))))

(ert-deftest elxiki-line-goto-next-sibling/none ()
  "`elxiki-line-goto-next-sibling' with no sibling."
  (with-temp-buffer
    (insert "  + A\n+ B\n  + C")
    (goto-char 2)
    (should-not (elxiki-line-goto-next-sibling))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-next-sibling/buffer-end ()
  "`elxiki-line-goto-next-sibling' at the end of a buffer."
  (with-temp-buffer
    (insert "+ A\n  + B\n\n")
    (goto-char 2)
    (should-not (elxiki-line-goto-next-sibling))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-next-sibling/invalid ()
  "`elxiki-line-goto-next-sibling' at an invalid line."
  (with-temp-buffer
    (insert "+ A\nBAD\n+ C")
    (goto-char 6)
    (should-not (elxiki-line-goto-next-sibling))
    (should (= 6 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/valid ()
  "`elxiki-line-goto-previous-sibling' with a sibling."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 10)
    (should (= 5 (elxiki-line-goto-previous-sibling)))
    (should (= 5 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/none ()
  "`elxiki-line-goto-previous-sibling' with no sibling."
  (with-temp-buffer
    (insert "  + A\n+ B\n  + C")
    (goto-char 12)
    (should-not (elxiki-line-goto-previous-sibling))
    (should (= 12 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/buffer-start ()
  "`elxiki-line-goto-previous-sibling' at the start of a buffer."
  (with-temp-buffer
    (insert "+ A\n")
    (goto-char 2)
    (should-not (elxiki-line-goto-previous-sibling))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/invalid ()
  "`elxiki-line-goto-next-sibling' at an invalid line."
  (with-temp-buffer
    (insert "+ A\nBAD\n+ C")
    (goto-char 6)
    (should-not (elxiki-line-goto-previous-sibling))
    (should (= 6 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/0 ()
  "`elxiki-line-goto-nth-sibling' 0."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 6)
    (should (= 5 (elxiki-line-goto-nth-sibling 0)))
    (should (= 5 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/2 ()
  "`elxiki-line-goto-nth-sibling' 2."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 2)
    (should (= 9 (elxiki-line-goto-nth-sibling 2)))
    (should (= 9 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/-2 ()
  "`elxiki-line-goto-nth-sibling' -2."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 10)
    (should (= 1 (elxiki-line-goto-nth-sibling -2)))
    (should (= 1 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/invalid ()
  "`elxiki-line-goto-nth-sibling' on an invalid line."
  (with-temp-buffer
    (insert "+ A\n+ B\nBAD")
    (goto-char 10)
    (should-not (elxiki-line-goto-nth-sibling -2))
    (should (= 10 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/-2-fail ()
  "`elxiki-line-goto-nth-sibling' with not enough previous siblings."
  (with-temp-buffer
    (insert "+ A\n  + B\n+ C")
    (goto-char 12)
    (should-not (elxiki-line-goto-nth-sibling -2))
    (should (= 12 (point)))))

(ert-deftest elxiki-line-goto-previous-sibling/2-fail ()
  "`elxiki-line-goto-nth-sibling' with not enough next siblings."
  (with-temp-buffer
    (insert "+ A\n  + B\n+ C")
    (goto-char 2)
    (should-not (elxiki-line-goto-nth-sibling 2))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-new-sibling/bad ()
  "`elxiki-line-goto-new-sibling' ending on a bad line."
  (with-temp-buffer
    (insert "+ A\nDEAD")
    (goto-char 2)
    (should (= 5 (elxiki-line-goto-new-sibling)))
    (should (= 5 (point)))))

(ert-deftest elxiki-line-goto-new-sibling/parent ()
  "`elxiki-line-goto-new-sibling' ending on a parent line."
  (with-temp-buffer
    (insert "  + A\n+ B")
    (goto-char 2)
    (should (= 7 (elxiki-line-goto-new-sibling)))
    (should (= 7 (point)))))

(ert-deftest elxiki-line-goto-new-sibling/buffer-end ()
  "`elxiki-line-goto-new-sibling' at the end of the buffer."
  (with-temp-buffer
    (insert "+ A\n  + B")
    (goto-char 2)
    (should (eq 'end (elxiki-line-goto-new-sibling)))
    (should (= 10 (point)))))

(ert-deftest elxiki-line-goto-new-sibling/many ()
  "`elxiki-line-goto-new-sibling' with many siblings."
  (with-temp-buffer
    (insert "+ A\n+ B\n  + c\n+ D\nEND")
    (goto-char 2)
    (should (= 19 (elxiki-line-goto-new-sibling)))
    (should (= 19 (point)))))

(ert-deftest elxiki-line-goto-new-sibling/invalid ()
  "`elxiki-line-goto-new-sibling' on an invalid line."
  (with-temp-buffer
    (insert "DEAD")
    (goto-char 2)
    (should-not (elxiki-line-goto-new-sibling))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-end-of-children/invalid ()
  "`elxiki-line-goto-end-of-children' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-goto-end-of-children))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-end-of-children/single-line ()
  "`elxiki-line-goto-end-of-children' on a single line."
  (with-temp-buffer
    (insert "+ A")
    (goto-char 2)
    (should (= 4 (elxiki-line-goto-end-of-children)))
    (should (= 4 (point)))))

(ert-deftest elxiki-line-goto-end-of-children/blank ()
  "`elxiki-line-goto-end-of-children' with trailing blanks."
  (with-temp-buffer
    (insert "+ A\n  + B\n    \n\n+ C")
    (goto-char 2)
    (should (= 11 (elxiki-line-goto-end-of-children)))
    (should (= 11 (point)))))

(ert-deftest elxiki-line-insert/empty ()
  "`elxiki-line-insert' with an empty string."
  (with-temp-buffer
    (insert "  ABC")
    (goto-char 2)
    (should-not (elxiki-line-insert ""))
    (should (= (point) 2))
    (should (string-equal "  ABC" (buffer-string)))))

(ert-deftest elxiki-line-insert/deindent ()
  "`elxiki-line-insert' with a string to deindent."
  (with-temp-buffer
    (insert "  ABC")
    (goto-char 2)
    (should (equal '(1 6) (elxiki-line-insert "  + DEF")))
    (should (= 1 (point)))
    (should (string-equal "+ DEF\n  ABC" (buffer-string)))))

(ert-deftest elxiki-line-insert/indent ()
  "`elxiki-line-insert' with a string to indent."
  (with-temp-buffer
    (insert "ABC")
    (goto-char 2)
    (should (equal '(1 18) (elxiki-line-insert "+ DEF\n  + IJK" 2)))
    (should (= 1 (point)))
    (should (string-equal "  + DEF\n    + IJK\nABC"
                          (buffer-string)))))

(ert-deftest elxiki-line-insert/drop-newline ()
  "`elxiki-line-insert' drops an extra newline at the end."
  (with-temp-buffer
    (insert "ABC")
    (goto-char 2)
    (should (equal '(1 18) (elxiki-line-insert "+ DEF\n  + IJK\n" 2)))
    (should (= 1 (point)))
    (should (string-equal "  + DEF\n    + IJK\nABC"
                          (buffer-string)))))

(ert-deftest elxiki-line-insert-after-children/invalid ()
  "`elxiki-line-insert-after-children' on an invalid line."
  (with-temp-buffer
    (insert "DEAD")
    (goto-char 2)
    (should-not (elxiki-line-insert-after-children "test"))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-insert-after-children/sibling ()
  "`elxiki-line-insert-after-children' a sibling."
  (with-temp-buffer
    (insert "  + A\n  + B\n  + C")
    (goto-char 2)
    (should (equal '(7 20) 
                   (elxiki-line-insert-after-children "+ X\n  + Y")))
    (should (= 7 (point)))
    (should (string-equal "  + A\n  + X\n    + Y\n  + B\n  + C"
                          (buffer-string)))))

(ert-deftest elxiki-line-insert-after-children/child ()
  "`elxiki-line-insert-after-children' a child."
  (with-temp-buffer
    (insert "  + A\n  + B\n  + C")
    (goto-char 2)
    (should (equal '(7 24)
                   (elxiki-line-insert-after-children "  + X\n    + Y" 2)))
    (should (= 7 (point)))
    (should (string-equal "  + A\n    + X\n      + Y\n  + B\n  + C"
                          (buffer-string)))))

(ert-deftest elxiki-line-insert-after-children/buffer-end ()
  "`elxiki-line-insert-after-children' should work at the end of the buffer."
  (with-temp-buffer
    (insert "  + A\n  + B\n  + C\n")
    (goto-char 15) ; C
    (should (equal '(19 36)
                   (elxiki-line-insert-after-children "  + X\n    + Y" 2)))
    (should (= 19 (point)))
    (should (string-equal "  + A\n  + B\n  + C\n    + X\n      + Y\n"
                          (buffer-string)))))

(ert-deftest elxiki-line-insert-after-siblings/invalid ()
  "`elxiki-line-insert-after-siblings' on an invalid line."
  (with-temp-buffer
    (insert "DEAD")
    (goto-char 2)
    (should-not (elxiki-line-insert-after-siblings "test"))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-insert-after-siblings/sibling ()
  "`elxiki-line-insert-after-siblings' a sibling."
  (with-temp-buffer
    (insert "  + A\n  + B\n    + C")
    (goto-char 2)
    (should (equal '(21 34)
                   (elxiki-line-insert-after-siblings "+ X\n  + Y")))
    (should (= 21 (point)))
    (should (string-equal "  + A\n  + B\n    + C\n  + X\n    + Y\n"
                          (buffer-string)))))

(ert-deftest elxiki-line-goto-sibling/fail ()
  "`elxiki-line-goto-sibling' without the proper sibling."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 6)
    (should-not (elxiki-line-goto-sibling "D" 'before))
    (should (= 6 (point)))))

(ert-deftest elxiki-line-goto-sibling/fail-2 ()
  "`elxiki-line-goto-sibling' with a line by itself."
  (with-temp-buffer
    (insert "+ A")
    (goto-char 2)
    (should-not (elxiki-line-goto-sibling "D" 'before))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-sibling/after ()
  "`elxiki-line-goto-sibling' with the sibling after it."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 2)
    (should (= 9 (elxiki-line-goto-sibling "C")))
    (should (= 9 (point)))))

(ert-deftest elxiki-line-goto-sibling/before ()
  "`elxiki-line-goto-sibling' with the sibling before it."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 5)
    (should (= 1 (elxiki-line-goto-sibling "A" 'before)))
    (should (= 1 (point)))))

(ert-deftest elxiki-line-goto-sibling/before-fail ()
  "`elxiki-line-goto-sibling' with the sibling before it, but
without the 'before flag."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 5)
    (should-not (elxiki-line-goto-sibling "A"))
    (should (= 5 (point)))))

(ert-deftest elxiki-line-goto-child/fail ()
  "`elxiki-line-goto-child' without the proper child."
  (with-temp-buffer
    (insert "+ A\n  + B\n  + C")
    (goto-char 2)
    (should-not (elxiki-line-goto-child "D"))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-child/fail-2 ()
  "`elxiki-line-goto-sibling' with a line by itself."
  (with-temp-buffer
    (insert "+ A")
    (goto-char 2)
    (should-not (elxiki-line-goto-child "D"))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-goto-child/find ()
  "`elxiki-line-goto-child' with the child after it."
  (with-temp-buffer
    (insert "+ A\n  + B\n  + C")
    (goto-char 2)
    (should (= 11 (elxiki-line-goto-child "C")))
    (should (= 11 (point)))))

(ert-deftest elxiki-line-follow-route/valid ()
  "`elxiki-line-follow-route' with a valid route."
  (with-temp-buffer
    (insert "+ A\n+ B\n  + C\n  + D")
    (goto-char 2)
    (should (= 15 (elxiki-line-follow-route '("B" "D"))))
    (should (= 15 (point)))))

(ert-deftest elxiki-line-follow-route/invalid ()
  "`elxiki-line-follow-route' with an invalid route."
  (with-temp-buffer
    (insert "+ A\n+ B\n  + C\n  + D")
    (goto-char 2)
    (should-not (elxiki-line-follow-route '("A" "D")))
    (should (= 2 (point)))))

(ert-deftest elxiki-line-follow-route/invalid-create ()
  "`elxiki-line-follow-route' with an invalid route."
  (with-temp-buffer
    (insert "+ A\n+ B\n  + C\n  + D")
    (goto-char 2)
    (should (= 5 (elxiki-line-follow-route '("A" "D") 'create)))
    (should (= 5 (point)))
    (should (string-equal "+ A\n  - D\n+ B\n  + C\n  + D"
                          (buffer-string)))))

(ert-deftest elxiki-line-get-ancestry/invalid ()
  "`elxiki-line-get-ancestry' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-get-ancestry))))

(ert-deftest elxiki-line-get-ancestry/valid ()
  "`elxiki-line-get-ancestry' on a valid line."
  (with-temp-buffer
    (insert "+ A\n+ B\n  + C\n  + D\n    + E\n    + F")
    (goto-char 32)
    (should (equal '(("+ " "B")
                     ("+ " "D")
                     ("+ " "F"))
                   (elxiki-line-get-ancestry)))))

(ert-deftest elxiki-line-get-ancestry/no-prefix ()
  "`elxiki-line-get-ancestry' on a line without a prefix."
  (with-temp-buffer
    (insert "A/")
    (goto-char 1)
    (should (equal '((nil "A/"))
                   (elxiki-line-get-ancestry)))))

(ert-deftest elxiki-line-get-ancestry/blank ()
  "`elxiki-line-get-ancestry' on a line with a blank after its parent."
  (with-temp-buffer
    (insert "+ A\n  \n  + B")
    (goto-char 10) ; B
    (should (equal '(("+ " "A")
                     ("+ " "B"))
                   (elxiki-line-get-ancestry)))))

(ert-deftest elxiki-line-get-branch-region/no-children ()
  "`elxiki-line-get-branch-region' with no children."
  (with-temp-buffer
    (insert "A\n+ A\nA")
    (goto-char 4)
    (should (equal '(3 7) (elxiki-line-get-branch-region)))))

(ert-deftest elxiki-line-get-branch-region/children ()
  "`elxiki-line-get-branch-region' with children."
  (with-temp-buffer
    (insert "+ A\n  + B\n+ C")
    (goto-char 2)
    (should (equal '(1 11) (elxiki-line-get-branch-region)))))

(ert-deftest elxiki-line-get-branch-region/invalid ()
  "`elxiki-line-get-branch-region' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-get-branch-region))))

(ert-deftest elxiki-line-get-children-region/no-children ()
  "`elxiki-line-get-children-region' with no children."
  (with-temp-buffer
    (insert "A\n+ A\nA")
    (goto-char 4)
    (should-not (elxiki-line-get-children-region))))

(ert-deftest elxiki-line-get-children-region/children ()
  "`elxiki-line-get-children-region' with children."
  (with-temp-buffer
    (insert "+ A\n  + B\n+ C")
    (goto-char 2)
    (should (equal '(5 11) (elxiki-line-get-children-region)))))

(ert-deftest elxiki-line-get-children-region/invalid ()
  "`elxiki-line-get-branch-region' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-get-children-region))))

(ert-deftest elxiki-line-get-siblings-region/invalid ()
  "`elxiki-line-get-siblings-region' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-get-siblings-region))))

(ert-deftest elxiki-line-get-siblings-region/none ()
  "`elxiki-line-get-siblings-region' with no siblings."
  (with-temp-buffer
    (insert "+ ABC")
    (goto-char 2)
    (should (equal (elxiki-line-get-siblings-region)
                   (elxiki-line-get-branch-region)))))

(ert-deftest elxiki-line-get-siblings-region/some ()
  "`elxiki-line-get-siblings-region' with siblings."
  (with-temp-buffer
    (insert "BAD\n+ P\n  + A\n  + B\n  + C\nBAD")
    (goto-char 18)
    (should (equal (list 9 27)
                   (elxiki-line-get-siblings-region)))))

(ert-deftest elxiki-line-get-above-siblings-region/invalid ()
  "`elxiki-line-get-above-siblings-region' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-get-above-siblings-region))))

(ert-deftest elxiki-line-get-above-siblings-region/none ()
  "`elxiki-line-get-above-siblings-region' with no above siblings."
  (with-temp-buffer
    (insert "+ A\n+ B")
    (goto-char 2)
    (should-not (elxiki-line-get-above-siblings-region))))

(ert-deftest elxiki-line-get-above-siblings-region/some ()
  "`elxiki-line-get-above-siblings-region' with above siblings."
  (with-temp-buffer
    (insert "+ P\n  + A\n  + B")
    (goto-char 15)
    (should (equal (list 5 11)
                   (elxiki-line-get-above-siblings-region)))))

(ert-deftest elxiki-line-get-below-siblings-region/invalid ()
  "`elxiki-line-get-below-siblings-region' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-get-below-siblings-region))))

(ert-deftest elxiki-line-get-below-siblings-region/none ()
  "`elxiki-line-get-below-siblings-region' with no below siblings."
  (with-temp-buffer
    (insert "+ A\n+ B")
    (goto-char 6)
    (should-not (elxiki-line-get-below-siblings-region))))

(ert-deftest elxiki-line-get-below-siblings-region/some ()
  "`elxiki-line-get-below-siblings-region' with below siblings."
  (with-temp-buffer
    (insert "+ P\n  + A\n  + B")
    (goto-char 7)
    (should (equal (list 11 16)
                   (elxiki-line-get-below-siblings-region)))))

(ert-deftest elxiki-line-delete-siblings/none ()
  "`elxiki-line-delete-siblings' with no siblings."
  (with-temp-buffer
    (insert "+ A\n  + B\n+ C")
    (goto-char 6)
    (elxiki-line-delete-siblings)
    (should (string-equal "+ A\n  + B\n+ C"
                          (buffer-string)))))

(ert-deftest elxiki-line-delete-siblings/some ()
  "`elxiki-line-delete-siblings' with no siblings."
  (with-temp-buffer
    (insert "+ A\n  + S\n  + B\n  + T\n+ C")
    (goto-char 14)
    (elxiki-line-delete-siblings)
    (should (string-equal "+ A\n  + B\n+ C"
                          (buffer-string)))))

(ert-deftest elxiki-line-set-prefix/remove-no-prefix ()
  "`elxiki-line-set-prefix' trying to remove a prefix that isn't there."
  (with-temp-buffer
    (insert "HERE IS A LINE")
    (goto-char 5)
    (elxiki-line-set-prefix)
    (should (string-equal "HERE IS A LINE"
                          (buffer-string)))))

(ert-deftest elxiki-line-set-prefix/remove-prefix ()
  "`elxiki-line-set-prefix' trying to remove a prefix."
  (with-temp-buffer
    (insert "  + Q")
    (goto-char 3)
    (elxiki-line-set-prefix)
    (should (string-equal "  Q"
                          (buffer-string)))))

(ert-deftest elxiki-line-set-prefix/add-prefix ()
  "`elxiki-line-set-prefix' trying to add a prefix."
  (with-temp-buffer
    (insert "HERE IS A LINE")
    (goto-char 5)
    (elxiki-line-set-prefix "+ ")
    (should (string-equal "+ HERE IS A LINE"
                          (buffer-string)))))

(ert-deftest elxiki-line-set-prefix/change-prefix ()
  "`elxiki-line-set-prefix' trying to add a prefix."
  (with-temp-buffer
    (insert "- HERE IS A LINE")
    (goto-char 5)
    (elxiki-line-set-prefix "+ ")
    (should (string-equal "+ HERE IS A LINE"
                          (buffer-string)))))

(ert-deftest elxiki-line-doto-siblings/invalid ()
  "`elxiki-line-doto-siblings' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-doto-siblings (lambda ())))))

(ert-deftest elxiki-line-doto-siblings/after ()
  "`elxiki-line-doto-siblings' on just the siblings after."
  (with-temp-buffer
    (insert "+ A\n  + B\n  + C\n  + D\n+ E")
    (goto-char 14)
    (elxiki-line-doto-siblings
     (lambda () (elxiki-line-set-prefix "- ")))
    (should (string-equal "+ A\n  + B\n  - C\n  - D\n+ E"
                          (buffer-string)))))

(ert-deftest elxiki-line-doto-siblings/all ()
  "`elxiki-line-doto-siblings' on all the siblings."
  (with-temp-buffer
    (insert "+ A\n  + B\n  + C\n  + D\n+ E")
    (goto-char 14)
    (elxiki-line-doto-siblings
     (lambda () (elxiki-line-set-prefix "- "))
     'before)
    (should (string-equal "+ A\n  - B\n  - C\n  - D\n+ E"
                          (buffer-string)))))

(ert-deftest elxiki-line-append-children/no-fun ()
  "`elxiki-line-append-children' with no line function."
  (with-temp-buffer
    (insert "+ A")
    (goto-char 2)
    (elxiki-line-append-children "+ B\n+ C")
    (should (string-equal "+ A\n  + B\n  + C\n" 
                          (buffer-string)))))

(ert-deftest elxiki-line-append-children/fun ()
  "`elxiki-line-append-children' with a line function."
  (with-temp-buffer
    (insert "+ A\n+ E\n+ F")
    (goto-char 2)
    (elxiki-line-append-children "B\nC\n" 
                                 (lambda ()
                                   (elxiki-line-goto-prefix)
                                   (insert "+ ")))
    (should (string-equal "+ A\n  + B\n  + C\n+ E\n+ F" 
                          (buffer-string)))))

(ert-deftest elxiki-line-match-siblings/invalid ()
  "`elxiki-line-match-siblings' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-match-siblings (lambda ())))))

(ert-deftest elxiki-line-match-siblings/after ()
  "`elxiki-line-match-siblings' on just the siblings after."
  (with-temp-buffer
    (insert "+ A\n  + B\n  - C\n  + D\n  - E")
    (goto-char 14)
    (should 
     (equal '(0 2)
            (elxiki-line-match-siblings
             (lambda () (string-equal "- " (elxiki-line-get-prefix))))))))

(ert-deftest elxiki-line-match-siblings/all ()
  "`elxiki-line-match-siblings' on all the siblings."
  (with-temp-buffer
    (insert "+ A\n  - B\n  + C\n  - D\n+ E")
    (goto-char 14)
    (should 
     (equal '(0 2)
            (elxiki-line-match-siblings
             (lambda () (string-equal "- " (elxiki-line-get-prefix)))
             'all)))))

(ert-deftest elxiki-line-filter-siblings/invalid ()
  "`elxiki-line-filter-siblings' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-filter-siblings (lambda ())))))

(ert-deftest elxiki-line-filter-siblings/all-restrict ()
  "`elxiki-line-filter-siblings' on all siblings, but restricted."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 2)
    (elxiki-line-filter-siblings 
     (lambda () (string-equal "- " (elxiki-line-get-prefix)))
     'restrict)
    (should (string-equal "+ A\n+ B\n+ C"
                          (buffer-string)))))

(ert-deftest elxiki-line-filter-siblings/all ()
  "`elxiki-line-filter-siblings' on all siblings."
  (with-temp-buffer
    (insert "+ A\n+ B\n+ C")
    (goto-char 2)
    (elxiki-line-filter-siblings 
     (lambda () (string-equal "- " (elxiki-line-get-prefix))))
    (should (string-equal ""
                          (buffer-string)))))

(ert-deftest elxiki-line-filter-siblings/after ()
  "`elxiki-line-filter-siblings' on siblings after."
  (with-temp-buffer
    (insert "+ A\n+ B\n- C")
    (goto-char 6)
    (elxiki-line-filter-siblings 
     (lambda () (string-equal "- " (elxiki-line-get-prefix))))
    (should (string-equal "+ A\n- C"
                          (buffer-string)))))

(ert-deftest elxiki-line-filter-siblings/all ()
  "`elxiki-line-filter-siblings' on all siblings."
  (with-temp-buffer
    (insert "+ A\n+ B\n- C")
    (goto-char 6)
    (elxiki-line-filter-siblings 
     (lambda () (string-equal "- " (elxiki-line-get-prefix)))
     nil
     'all)
    (should (string-equal "- C"
                          (buffer-string)))))

(ert-deftest elxiki-line-promote/invalid ()
  "`elxiki-line-promote' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-promote))))

(ert-deftest elxiki-line-promote/no-parent ()
  "`elxiki-line-promote' on a line without a parent."
  (with-temp-buffer
    (insert "+ A\n+ B")
    (goto-char 2)
    (should-not (elxiki-line-promote))))

(ert-deftest elxiki-line-promote/children ()
  "`elxiki-line-promote' on a line with children."
  (with-temp-buffer
    (insert "+ A\n  + B\n  + C\n    + D\n    + E\n")
    (goto-char 13) ; C
    (should (elxiki-line-promote))
    (should (string-equal "+ A\n  + B\n+ C\n  + D\n  + E\n"
                          (buffer-string)))))

(ert-deftest elxiki-line-replace-parent/invalid ()
  "`elxiki-line-replace-parent' on an invalid line."
  (with-temp-buffer
    (insert "BAD")
    (goto-char 2)
    (should-not (elxiki-line-replace-parent))))

(ert-deftest elxiki-line-replace-parent/no-parent ()
  "`elxiki-line-replace-parent' on a line without a parent."
  (with-temp-buffer
    (insert "+ A\n+ B")
    (goto-char 2)
    (should-not (elxiki-line-replace-parent))))

(ert-deftest elxiki-line-replace-parent/children ()
  "`elxiki-line-replace-parent' on a line with children."
  (with-temp-buffer
    (insert "+ A\n  + B\n  + C\n    + D\n    + E\n")
    (goto-char 13) ; C
    (should (elxiki-line-replace-parent))
    (should (string-equal "+ C\n  + D\n  + E\n"
                          (buffer-string)))))

