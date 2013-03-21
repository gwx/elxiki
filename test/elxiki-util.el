(require 'elxiki-util)
(require 'ert)

(defvar elxiki-util-test--string-1 
  "ABCD\nEFGH\nIJKL\nMNOP\nQRST\nUVWX\nYZ")

(ert-deftest elxiki-change-line/nil ()
  "`elxiki-change-line' with no arguments.
Should change point to beginning of line."
  (with-temp-buffer
    (insert elxiki-util-test--string-1)
    (goto-char 13)
    (let ((start-line (line-number-at-pos)))
      (should (elxiki-change-line))
      (should (bolp))
      (should (= start-line (line-number-at-pos))))))

(ert-deftest elxiki-change-line/2 ()
  "`elxiki-change-line' with going forward."
  (with-temp-buffer
    (insert elxiki-util-test--string-1)
    (goto-char 13)
    (let ((start-line (line-number-at-pos)))
      (should (elxiki-change-line 2))
      (should (bolp))
      (should (= (+ 2 start-line) (line-number-at-pos))))))

(ert-deftest elxiki-change-line/-2 ()
  "`elxiki-change-line' with going backward."
  (with-temp-buffer
    (insert elxiki-util-test--string-1)
    (goto-char 13)
    (let ((start-line (line-number-at-pos)))
      (should (elxiki-change-line -2))
      (should (bolp))
      (should (= (- start-line 2) (line-number-at-pos))))))

(ert-deftest elxiki-change-line/buffer-end ()
  "`elxiki-change-line' at end of buffer.
Should return nil because it cannot go to next line."
  (with-temp-buffer
    (insert elxiki-util-test--string-1)
    (goto-char 31)
    (should-not (elxiki-change-line 1))
    (should (= (point) 31))))

(ert-deftest elxiki-change-line/buffer-start ()
  "`elxiki-change-line' at end of buffer.
Should return nil because it cannot go to next line."
  (with-temp-buffer
    (insert elxiki-util-test--string-1)
    (goto-char 5)
    (should-not (elxiki-change-line -1))
    (should (= (point) 5))))

(ert-deftest elxiki-change-indentation/0 ()
  "`elxiki-change-indentation' with 0 argument."
  (with-temp-buffer
    (insert "  Hallo\n")
    (goto-char 4)
    (should (elxiki-change-indentation 0))
    (should (= 2 (current-indentation)))))

(ert-deftest elxiki-change-indentation/3-on-0 ()
  "`elxiki-change-indentation' with 3 argument on 0 line."
  (with-temp-buffer
    (insert "Hallo\n")
    (goto-char 3)
    (should (elxiki-change-indentation 3))
    (should (= 3 (current-indentation)))))

(ert-deftest elxiki-change-indentation/3-on-3 ()
  "`elxiki-change-indentation' with 3 argument on 3 line."
  (with-temp-buffer
    (insert "   Hallo\n")
    (goto-char 3)
    (should (elxiki-change-indentation 3))
    (should (= 6 (current-indentation)))))

(ert-deftest elxiki-change-indentation/-2-on-3 ()
  "`elxiki-change-indentation' with -2 argument on 3 line."
  (with-temp-buffer
    (insert "   Hallo\n")
    (goto-char 5)
    (should (elxiki-change-indentation -2))
    (should (= 1 (current-indentation)))))

(ert-deftest elxiki-change-indentation/-2-on-1 ()
  "`elxiki-change-indentation' with -2 argument on 1 line."
  (with-temp-buffer
    (insert " Hallo\n")
    (goto-char 5)
    (should-error (elxiki-change-indentation -2)
                  :type 'elxiki-negative-indent-error)
    (should (= 0 (current-indentation)))))

(ert-deftest elxiki-change-indentation/-2-on-1-no-error ()
  "`elxiki-change-indentation' with -2 argument on 1 line."
  (with-temp-buffer
    (insert " Hallo\n")
    (goto-char 5)
    (should (elxiki-change-indentation -2 'no-error))
    (should (= 0 (current-indentation)))))

(ert-deftest elxiki-normalize-indentation/increase ()
  "Use `elxiki-normalize-indentation' to increase indentation."
  (with-temp-buffer
    (insert "Go\n  Do\n  This\nThing")
    (elxiki-normalize-indentation 2 13 3)
    (goto-char (point-min))
    (should (= 3 (current-indentation)))
    (forward-line 1)
    (should (= 5 (current-indentation)))
    (forward-line 1)
    (should (= 5 (current-indentation)))
    (forward-line 1)
    (should (= 0 (current-indentation)))))

(ert-deftest elxiki-normalize-indentation/decrease ()
  "Use `elxiki-normalize-indentation' to decrease indentation."
  (with-temp-buffer
    (insert "  Go\n    Do\n    This\n Thing")
    (elxiki-normalize-indentation 2 17 0)
    (goto-char (point-min))
    (should (= 0 (current-indentation)))
    (forward-line 1)
    (should (= 2 (current-indentation)))
    (forward-line 1)
    (should (= 2 (current-indentation)))
    (forward-line 1)
    (should (= 1 (current-indentation)))))

(ert-deftest elxiki-normalize-indentation/buffer-edge ()
  "Use `elxiki-normalize-indentation' to increase indentation."
  (with-temp-buffer
    (insert "Go\n  Do")
    (elxiki-normalize-indentation 1 8 2)
    (goto-char (point-min))
    (should (= 2 (current-indentation)))
    (forward-line 1)
    (should (= 4 (current-indentation)))))

(ert-deftest elxiki-normalize-indentation/error ()
  "Use `elxiki-normalize-indentation' with improper indentation."
  (with-temp-buffer
    (insert "  Go\nDo")
    (should-error (elxiki-normalize-indentation 2 5 -1)
                  :type 'elxiki-negative-indent-error)))

(ert-deftest elxiki-normalize-indentation/no-error ()
  "Use `elxiki-normalize-indentation' with improper indentation, ignore error."
  (with-temp-buffer
    (insert "  Go\nDo")
    (elxiki-normalize-indentation 2 5 -1 'no-error)
    (goto-char (point-min))
    (should (= 0 (current-indentation)))
    (forward-line 1)
    (should (= 0 (current-indentation)))))

(ert-deftest elxiki-strip-end-fslash ()
  (should (string-equal "ABC"
                        (elxiki-strip-end-fslash "ABC/  ")))
  (should (string-equal "ABC"
                        (elxiki-strip-end-fslash "ABC/")))
  (should (string-equal "ABC  "
                        (elxiki-strip-end-fslash "ABC  "))))

(ert-deftest elxiki-name-equal ()
  (should (elxiki-name-equal "ABC/ " "ABC"))
  (should (elxiki-name-equal "ABC" "ABC"))
  (should-not (elxiki-name-equal "ABD" "ABC/")))

(ert-deftest elxiki-narrow-to-lines ()
  (with-temp-buffer
    (insert "AAA\nBBB\nCCC")
    (elxiki-narrow-to-lines 6 11)
    (should (= (point-min) 5))
    (should (= (point-max) 12))))

(ert-deftest elxiki-doto-lines ()
  (with-temp-buffer
    (insert "AAA\nBBB\nCCC")
    (elxiki-doto-lines (lambda () (insert "+ "))
                       6 11)
    (should (string-equal "AAA\n+ BBB\n+ CCC"
                          (buffer-string)))))

(ert-deftest elxiki-path-root ()
  (should (string-equal "root/" (elxiki-path-root "root/sub/sub2")))
  (should (string-equal "/" (elxiki-path-root "/home")))
  (should (string-equal "whole-path" (elxiki-path-root "whole-path"))))
