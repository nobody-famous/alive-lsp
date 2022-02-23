(defpackage :alive/session
    (:use :cl)
    (:export :add-listener
             :create
             :listener
             :set-started
             :set-stopped)
    (:local-nicknames (:logger :alive/logger)))

(in-package :alive/session)


(defclass session ()
    ((files :accessor files
            :initform (make-hash-table :test 'equalp)
            :initarg :files)
     (logger :accessor logger
             :initform nil
             :initarg :logger)
     (initialized :accessor initialized
                  :initform nil
                  :initarg :initialized)))


(defclass network-session (session)
    ((conn :accessor conn
           :initform nil
           :initarg :conn)))


(defun create (&key logger conn)
    (make-instance 'session
                   :logger logger
                   :conn conn))


(defmethod set-started ((obj session))
    (setf (running obj) T))


(defmethod set-stopped ((obj session))
    (setf (running obj) nil))


(defmethod add-listener ((obj session) (to-add listener))
    (push to-add (listeners obj)))
