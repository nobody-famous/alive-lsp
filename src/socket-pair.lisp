(defpackage :alive/socket-pair
    (:use :cl)
    (:export :create
             :reader
             :writer))

(in-package :alive/socket-pair)


(defclass socket-pair ()
    ((reader :accessor reader
             :initform nil
             :initarg :reader)
     (writer :accessor writer
             :initform nil
             :initarg :writer)))


(defun create ()
    (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address T))
           (port (usocket:get-local-port listener))
           (write-side (usocket:socket-connect "127.0.0.1" port))
           (read-side (usocket:socket-accept listener)))
        (usocket:socket-close listener)

        (make-instance 'socket-pair
                       :reader read-side
                       :writer write-side)))