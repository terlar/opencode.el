;;; opencode-diff-parser-test.el --- Tests for opencode diff parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; ERT tests for `opencode-diff-parser.el'.
;;
;; Run from the project root with:
;;
;;   emacs -Q -L . -batch \
;;     -l opencode-diff-parser.el \
;;     -l opencode-diff-parser-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'opencode-diff-parser)

(defun opencode-diff-parser-test--normalize (result)
  "Return RESULT sorted by file name and range start.

`opencode-diff->source-line-ranges' stores intermediate results in a
hash table, so file order should not be part of the test contract."
  (sort
   (mapcar
    (lambda (entry)
      (cons (car entry)
            (sort (copy-sequence (cdr entry))
                  (lambda (a b)
                    (< (car a) (car b))))))
    result)
   (lambda (a b)
     (string< (car a) (car b)))))

(defun opencode-diff-parser-test--equal (actual expected)
  "Return non-nil when ACTUAL and EXPECTED are equal after normalization."
  (equal (opencode-diff-parser-test--normalize actual)
         (opencode-diff-parser-test--normalize expected)))

(ert-deftest opencode-diff-parser-readme-org-regression ()
  "Return the changed README.org line from the pasted regression case.

This uses a bare empty line inside the hunk, because that was the case
that made the previous scanner leave the hunk before the added line."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "
Index: /home/user/opencode.el/README.org
===================================================================
--- /home/user/opencode.el/README.org
+++ /home/user/opencode.el/README.org
@@ -220,9 +220,9 @@
 questions, selecting an option will immediately submit that response. For
 multiple choice questions, select the options you want and then press =C-c C-c= to
 submit.

-* Security
+* Insecurity
 Opencode's permission system, especially for bash, is not nearly as fine-grained
 as claude's. However, I don't think this really matters. Any coding agent with
 the ability to write and execute code can execute potentially malicious code by
 design and should be untrusted. One approach I'd recommend is to develop inside
")
    '(("/home/user/opencode.el/README.org" . ((224 . 224)))))))

(ert-deftest opencode-diff-parser-agents-md-regression ()
  "Return the changed AGENTS.md line from the pasted regression case.

This also uses a bare empty line inside the hunk."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "
/home/user/opencode.el/AGENTS.md
===================================================================
--- /home/user/opencode.el/AGENTS.md
+++ /home/user/opencode.el/AGENTS.md
@@ -1,9 +1,9 @@
 # AGENTS.md - opencode.el

 Emacs frontend for OpenCode LLM coding assistant server.

-# Tool use
+# MCP use

 Interact with the user's emacs process to perform introspection, find
 information about functions, values of variables etc. Use it to run and verify
 that code you write is working correctly.
")
    '(("/home/user/opencode.el/AGENTS.md" . ((5 . 5)))))))

(ert-deftest opencode-diff-parser-standard-modification ()
  "A normal remove/add pair records only the post-edit added line."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- foo.txt
+++ foo.txt
@@ -1,5 +1,5 @@
 one
 two
-old
+new
 four
 five
")
    '(("foo.txt" . ((3 . 3)))))))

(ert-deftest opencode-diff-parser-contiguous-additions-are-merged ()
  "Adjacent added lines are returned as one range."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- foo.txt
+++ foo.txt
@@ -1,3 +1,5 @@
 one
+two
+three
 four
 five
")
    '(("foo.txt" . ((2 . 3)))))))

(ert-deftest opencode-diff-parser-separated-additions-are-not-merged ()
  "Added lines separated by context are returned as separate ranges."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- foo.txt
+++ foo.txt
@@ -1,5 +1,7 @@
 one
+two
 three
+four
 five
 six
 seven
")
    '(("foo.txt" . ((2 . 2) (4 . 4)))))))

(ert-deftest opencode-diff-parser-deletion-only ()
  "Deletion-only hunks return the post-edit deletion point with nil end."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- foo.txt
+++ foo.txt
@@ -3,2 +3,0 @@
-old 3
-old 4
")
    (list (cons "foo.txt" (list (cons 3 nil)))))))

(ert-deftest opencode-diff-parser-deletion-run-followed-by-addition-is-modification ()
  "A remove/add edit run is a modification, not a deletion-only range."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- foo.txt
+++ foo.txt
@@ -10,4 +10,4 @@
 context before
-old one
-old two
+new one
+new two
 context after
")
    '(("foo.txt" . ((11 . 12)))))))

(ert-deftest opencode-diff-parser-new-file ()
  "A new file records added ranges from line one."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- /dev/null
+++ new.txt
@@ -0,0 +1,2 @@
+hello
+world
")
    '(("new.txt" . ((1 . 2)))))))

(ert-deftest opencode-diff-parser-multiple-files ()
  "Multiple files are reported independently."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- one.txt
+++ one.txt
@@ -1,2 +1,3 @@
 one
+two
 three
--- two.txt
+++ two.txt
@@ -4,3 +4,3 @@
 four
-five
+FIVE
 six
")
    '(("one.txt" . ((2 . 2)))
      ("two.txt" . ((5 . 5)))))))

(ert-deftest opencode-diff-parser-content-lines-starting-with-file-header-markers ()
  "Inside hunks, +++ and --- are content lines, not file headers."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- foo.txt
+++ foo.txt
@@ -1,3 +1,3 @@
 before
---- old heading
++++ new heading
 after
")
    '(("foo.txt" . ((2 . 2)))))))

(ert-deftest opencode-diff-parser-no-newline-marker-does-not-split-edit-run ()
  "The no-newline marker should not split adjacent added lines."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- foo.txt
+++ foo.txt
@@ -1,1 +1,2 @@
-old
+new
\\ No newline at end of file
+extra
")
    '(("foo.txt" . ((1 . 2)))))))

(ert-deftest opencode-diff-parser-quoted-file-names-with-special-characters ()
  "Parse quoted diff file headers with escaped special characters."
  (should
   (opencode-diff-parser-test--equal
    (opencode-diff->source-line-ranges
     "--- \"dir/file name [draft] #1 \\\"quoted\\\".txt\"
+++ \"dir/file name [draft] #1 \\\"quoted\\\".txt\"
@@ -1,3 +1,3 @@
 alpha
-old
+new
 omega
")
    '(( "dir/file name [draft] #1 \"quoted\".txt" . ((2 . 2)))))))

(ert-deftest opencode-diff-parser-two-files-patch-header-test ()
  "Parse the patches of the v2 server, which have an Index line and no prefixes."
  (should
   (equal
    (opencode-diff->source-line-ranges
     (concat "Index: dir/a.txt\n"
             "===================================================================\n"
             "--- dir/a.txt\n"
             "+++ dir/a.txt\n"
             "@@ -1,3 +1,3 @@\n"
             " one\n"
             "-two\n"
             "+three\n"
             " four\n"))
    '(("dir/a.txt" (2 . 2))))))

(provide 'opencode-diff-parser-test)
;;; opencode-diff-parser-test.el ends here
