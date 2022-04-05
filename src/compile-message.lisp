(defpackage :alive/compile-message
    (:use :cl)
    (:export :create))

(in-package :alive/compile-message)


(defclass compile-message ()
    ((severity :accessor severity
               :initform nil
               :initarg :severity)
     (location :accessor location
               :initform nil
               :initarg :location)
     (message :accessor message
              :initform nil
              :initarg :message)))


(defmethod print-object ((obj compile-message) out)
    (format out "{severity: ~A; location: ~A; message: ~A}"
            (severity obj)
            (location obj)
            (message obj)))


(defun create (&key severity location message)
    (make-instance 'compile-message
                   :severity severity
                   :location location
                   :message message))
