(defpackage :alive/sbcl/threads
    (:use :cl)
    (:export :get-thread-id))

(in-package :alive/sbcl/threads)


(defun get-thread-id (thread)
    (bt:thread-name thread))
