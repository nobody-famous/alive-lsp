(defpackage :alive/streams
    (:use :cl)
    (:export :add-listener
             :set-listener
             :set-in-listener
             :set-out-listener
             :eof-p
             :flush-stream
             :flush-out-stream
             :make-io-stream
             :make-output-stream
             :make-input-stream))

(in-package :alive/streams)


(defun make-io-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:io-stream))


(defun make-output-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:output-stream))


(defun make-input-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:input-stream))


(defun flush-stream (obj)
    #+sbcl (alive/sbcl/streams:flush-buffer obj))


(defun flush-out-stream (obj)
    #+sbcl (alive/sbcl/streams:flush-out-buffer obj))


(defun add-listener (obj listener)
    #+sbcl (alive/sbcl/streams:add-listener obj listener))


(defun set-listener (obj listener)
    #+sbcl (alive/sbcl/streams:set-listener obj listener))


(defun set-in-listener (obj listener)
    #+sbcl (alive/sbcl/streams:set-in-listener obj listener))


(defun set-out-listener (obj listener)
    #+sbcl (alive/sbcl/streams:set-out-listener obj listener))


(defun eof-p (obj)
    #+sbcl (alive/sbcl/streams:eof-p obj))