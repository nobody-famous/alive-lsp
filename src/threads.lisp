(defpackage :alive/threads
    (:use :cl)
    (:export :list-all
             :kill))

(in-package :alive/threads)


(defclass thread ()
    ((id :accessor id
         :initform nil
         :initarg :id)
     (name :accessor name
           :initform nil
           :initarg :name)))


(defun list-all ()
    (mapcar (lambda (thread)
                (make-instance 'thread
                               :id (sxhash thread)
                               :name (bt:thread-name thread)))
            (bt:all-threads)))


(defun kill (thread-hash)
    (format T "KILL THREAD ~A~%" thread-hash))
