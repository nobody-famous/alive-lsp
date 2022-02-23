(defpackage :alive/state
    (:use :cl)
    (:export :add-listener
             :create
             :destroy
             :listener
             :send-msg
             :get-input-stream
             :get-file-text
             :set-file-text
             :set-initialized)
    (:local-nicknames (:logger :alive/logger)))

(in-package :alive/state)


(defclass state ()
    ((files :accessor files
            :initform (make-hash-table :test 'equalp)
            :initarg :files)
     (logger :accessor logger
             :initform nil
             :initarg :logger)
     (initialized :accessor initialized
                  :initform nil
                  :initarg :initialized)))


(defclass network-state (state)
    ((conn :accessor conn
           :initform nil
           :initarg :conn)))


(defun create (&key logger conn)
    (make-instance 'network-state
                   :logger logger
                   :conn conn))


(defmethod destroy ((obj network-state))
    (when (conn obj)
          (usocket:socket-close (conn obj))
          (setf (conn obj) NIL)))


(defmethod set-initialized ((obj state) value)
    (setf (initialized obj) value))


(defmethod set-file-text ((obj state) uri text)
    (setf (gethash uri (files obj)) text))


(defmethod get-file-text ((obj state) uri)
    (gethash uri (files obj)))


(defmethod send-msg ((obj network-state) msg)
    (format T "state send-msg ~A~%" obj)
    (write-string msg (usocket:socket-stream (conn obj)))
    (force-output (usocket:socket-stream (conn obj))))


(defmethod get-input-stream ((obj network-state))
    (usocket:socket-stream (conn obj)))
