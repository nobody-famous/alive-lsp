(defpackage :alive/threads
    (:use :cl)
    (:export :list-all
             :kill
             :thread-not-found
             :id))

(in-package :alive/threads)


(define-condition thread-not-found (error)
    ((id :accessor id
         :initform nil
         :initarg :id))
    (:report (lambda (condition stream) (format stream "Thread ~A Not Found" (id condition)))))


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
    (let ((thread (reduce (lambda (acc thread)
                              (if acc
                                  acc
                                  (when (= thread-hash (sxhash thread))
                                        thread))
                              acc)
                          (bt:all-threads)
                          :initial-value nil)))
        (unless thread
                (error (make-condition 'thread-not-found :id thread-hash)))

        (bt:destroy-thread thread)))
