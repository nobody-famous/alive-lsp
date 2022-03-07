(defpackage :alive/test/compat/sbcl/compile
    (:use :cl)
    (:export :run-all)

    (:local-nicknames (:astreams :alive/streams)
                      (:file :alive/file)
                      (:run :alive/test/harness/run)
                      (:check :alive/test/harness/check)))

(in-package :alive/test/compat/sbcl/compile)


(defun compile-foo ()
    (run:test "Compile foo.lisp Test"
              (lambda ()
                  (let ((msgs (file:do-compile "test/files/compile/foo.lisp"
                                               :stdout-fn (lambda (data)
                                                              (declare (ignore data)))
                                               :stderr-fn (lambda (data)
                                                              (declare (ignore data))))))
                      (check:are-equal 5 (length msgs))))))


(defun load-foo ()
    (run:test "Load foo.lisp Test"
              (lambda ()
                  (let ((msgs (file:do-load "test/files/compile/foo.lisp"
                                            :stdout-fn (lambda (data)
                                                           (declare (ignore data)))
                                            :stderr-fn (lambda (data)
                                                           (declare (ignore data))))))

                      (check:are-equal 9 (length msgs))))))


(defun compile-broken ()
    (run:test "Compile broken.lisp Test"
              (lambda ()
                  (let ((msgs (file:try-compile "test/files/compile/parens.lisp"
                                                :stdout-fn (lambda (data)
                                                               (format T "~A~%" data))
                                                :stderr-fn (lambda (data)
                                                               (declare (ignore data))))))

                      (loop :for msg :in msgs :do
                                (format T "~A~%" msg))
                      (check:are-equal 5 (length msgs))))))


(defun run-all ()
    (run:suite "SBCL Compile Tests"
               (lambda ()
                   (compile-foo)
                   (load-foo))))
