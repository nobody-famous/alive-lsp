(defpackage :alive/test/compat/sbcl/compile
    (:use :cl)
    (:export :run-all)

    (:local-nicknames (:astreams :alive/sys/streams)
                      (:file :alive/file)))

(in-package :alive/test/compat/sbcl/compile)


(defun compile-foo ()
    (clue:test "Compile foo.lisp Test"
        (let ((msgs (file:do-compile "test/files/compile/foo.lisp")))
            (clue:check-equal :expected 5
                              :actual (length msgs)))))


(defun load-foo ()
    (clue:test "Load foo.lisp Test"
        (let ((msgs (file:do-load "test/files/compile/foo.lisp")))

            (clue:check-equal :expected 9
                              :actual (length msgs)))))


(defun compile-broken ()
    (clue:test "Compile broken.lisp Test"
        (let ((msgs (file:try-compile "test/files/compile/broken.lisp")))

            (loop :for msg :in msgs :do
                      (format T "~A~%" msg))
            (clue:check-equal :expected 5
                              :actual (length msgs)))))


(defun compile-parens ()
    (clue:test "Compile parens.lisp Test"
        (let ((msgs (file:try-compile "test/files/compile/parens.lisp")))

            (loop :for msg :in msgs :do
                      (format T "~A~%" msg))
            (clue:check-equal :expected 5
                              :actual (length msgs)))))


(defun run-all ()
    (clue:suite "SBCL Compile Tests"
        (compile-foo)
        (load-foo)))
