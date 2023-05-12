(defpackage :alive/test/compile
    (:use :cl)
    (:export :run-all))

(in-package :alive/test/compile)


(defun do-compile (fn path)
    (let ((msgs (funcall fn path
                    :stdout-fn (lambda (data)
                                   (declare (ignore data)))
                    :stderr-fn (lambda (data)
                                   (declare (ignore data))))))

        (mapcar (lambda (msg) (gethash "severity" msg))
                msgs)))


(defun do-load (path)
    (unwind-protect
            (handler-case
                    (let ((msgs (alive/file:do-load path
                                                    :stdout-fn (lambda (data)
                                                                   (declare (ignore data)))
                                                    :stderr-fn (lambda (data)
                                                                   (declare (ignore data))))))

                        (mapcar (lambda (msg) (gethash "severity" msg))
                                msgs))
                (error (e)
                    (declare (ignore e))
                    (list alive/types:*sev-error*))
                (T (e)
                   (declare (ignore e))
                   nil))
        (delete-package :alive/test/files)))


(defun test-try-compile ()
    (clue:test "Try Compile"
        (clue:check-equal :actual (do-compile 'alive/file:try-compile "test/files/compile/broken.lisp")
                          :expected (list alive/types:*sev-error*
                                          alive/types:*sev-info*
                                          alive/types:*sev-warn*
                                          alive/types:*sev-warn*))))


(defun test-compile ()
    (clue:test "Compile"
        (clue:check-equal :actual (do-compile 'alive/file:do-compile "test/files/compile/broken.lisp")
                          :expected (list alive/types:*sev-error*
                                          alive/types:*sev-info*
                                          alive/types:*sev-warn*
                                          alive/types:*sev-warn*))))


(defun test-load-errors ()
    (clue:test "Load File Errors"
        (clue:check-equal :actual (do-load "test/files/compile/load-errors.lisp")
                          :expected (list alive/types:*sev-error*))))


(defun test-load-ok ()
    (clue:test "Load File OK"
        (clue:check-equal :actual (do-load "test/files/compile/load-ok.lisp")
                          :expected (list))))


(defun run-all ()
    (clue:suite "Compile Tests"
        (test-try-compile)
        (test-compile)
        (test-load-ok)
        (test-load-errors)))
