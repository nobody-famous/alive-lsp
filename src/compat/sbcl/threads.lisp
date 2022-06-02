(defpackage :alive/sbcl/threads
    (:use :cl)
    (:export :get-stack-trace
             :get-thread-id))

(in-package :alive/sbcl/threads)


(defun get-thread-id (thread)
    (bt:thread-name thread))


(defun get-stack-trace ()
    (sb-debug:list-backtrace))
