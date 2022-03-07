(defpackage :alive/streams
    (:use :cl)
    (:export :add-listener
             :eof-p
             :make-stream))

(in-package :alive/streams)


(defun make-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:rt-stream))


(defun add-listener (obj listener)
    #+sbcl (alive/sbcl/streams:add-listener obj listener))


(defun eof-p (obj)
    #+sbcl (alive/sbcl/streams:eof-p obj))
