(defpackage :alive/streams
    (:use :cl)
    (:export :eof-p
             :make-stream))

(in-package :alive/streams)


(defun make-stream (&key stdout)
    #+sbcl (make-instance 'alive/sbcl/streams:rt-stream :stdout stdout))


(defun eof-p (obj)
    #+sbcl (alive/sbcl/streams:eof-p obj))
