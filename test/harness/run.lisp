(defpackage :alive/test/harness/run
    (:use :cl)
    (:export :suite
             :test)
    (:local-nicknames (:fmt :alive/test/harness/formatting)))

(in-package :alive/test/harness/run)


(defun suite (label fn)
    (fmt:print-header label)
    (funcall fn))


(defun test (label fn)
    (let ((result (handler-case (progn
                                 (funcall fn)
                                 nil)
                      (error (c) (format nil "~A" c)))))

        (format T "~A [~A]~%" label (if result
                                        (format nil "FAILED: ~A" result)
                                        "OK"))))
