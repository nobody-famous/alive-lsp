(defpackage :alive/streams
    (:use :cl)
    (:export :add-listener
             :eof-p
             :flush-stream
             :make-stream))

(in-package :alive/streams)


(defun make-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:output-stream))


(defun flush-stream (obj)
    #+sbcl (alive/sbcl/streams:flush-buffer obj))


(defun add-listener (obj listener)
    #+sbcl (alive/sbcl/streams:add-listener obj listener))


(defun eof-p (obj)
    #+sbcl (alive/sbcl/streams:eof-p obj))
