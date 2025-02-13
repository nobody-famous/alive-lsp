(defpackage :alive/test/asdf/load
    (:use :cl)
    (:export :run-all))

(in-package :alive/test/asdf/load)


(defun load-test-asdf ()
    (load "test/asdf/foo.asd"))


(defun test-list-systems ()
    (clue:test "List Systems"
        (clue:check-exists (find "foo" (alive/sys/asdf:list-all) :test #'string=))))


(defun test-load-system ()
    (clue:test "Load System"
        (handler-case
                (alive/sys/asdf:load-system :name "foo"
                                            :force T
                                            :stdout-fn (lambda (data)
                                                           (declare (ignore data)))
                                            :stderr-fn (lambda (data)
                                                           (declare (ignore data))))
            (error (c) (declare (ignore c))))

        (alive/sys/asdf:load-system :name "foo/2"
                                    :stdout-fn (lambda (data)
                                                   (declare (ignore data)))
                                    :stderr-fn (lambda (data)
                                                   (declare (ignore data))))

        (clue:check-exists (find "foo" (alive/sys/asdf:list-all) :test #'string=))))


(defun run-all ()
    (load-test-asdf)

    (clue:suite "ASDF Tests"
        (test-list-systems)
        (test-load-system)))
