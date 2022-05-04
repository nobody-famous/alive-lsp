(defpackage :alive/streams
    (:use :cl)
    (:export :add-listener
             :add-to-input
             :set-listener
             :eof-p
             :flush-stream
             :make-output-stream
             :make-input-stream))

(in-package :alive/streams)


(defun make-output-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:output-stream))


(defun make-input-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:input-stream))


(defun flush-stream (obj)
    #+sbcl (alive/sbcl/streams:flush-buffer obj))


(defun add-listener (obj listener)
    #+sbcl (alive/sbcl/streams:add-listener obj listener))


(defun set-listener (obj listener)
    #+sbcl (alive/sbcl/streams:set-listener obj listener))


(defun add-to-input (obj data)
    #+sbcl (alive/sbcl/streams:add-to-input obj data))


(defun eof-p (obj)
    #+sbcl (alive/sbcl/streams:eof-p obj))