(defpackage :alive/threads
    (:use :cl)
    (:export :list-all
             :kill
             :thread-not-found
             :get-thread-id
             :id)
    (:local-nicknames (:logger :alive/logger)))

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


(defun get-thread-id (thread)
    #+sbcl (alive/sbcl/threads:get-thread-id thread))


(defun list-all ()
    (mapcar (lambda (thread)
                (make-instance 'thread
                               :id (get-thread-id thread)
                               :name (bt:thread-name thread)))
            (bt:all-threads)))


(defun kill (thread-hash)
    (let ((thread (reduce (lambda (acc thread)
                              (or acc
                                  (when (= thread-hash (get-thread-id thread))
                                        thread)))
                          (bt:all-threads)
                          :initial-value nil)))

        (unless thread
                (error (make-condition 'thread-not-found :id thread-hash)))

        (bt:destroy-thread thread)))
