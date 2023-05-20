(defpackage :alive/threads
    (:use :cl)
    (:export :list-all
             :kill
             :thread-not-found
             :get-thread-id
             :find-by-id
             :id))

(in-package :alive/threads)


(define-condition thread-not-found (error)
        ((id :accessor id
             :initform nil
             :initarg :id))
    (:report (lambda (condition stream) (format stream "Thread ~A Not Found" (id condition)))))


(defun get-thread-id (thread)
    #+sbcl (alive/sbcl/threads:get-thread-id thread))


(defun list-all ()
    (mapcar (lambda (thread)
                (list (cons :id (get-thread-id thread))
                      (cons :name (bt:thread-name thread))))
            (bt:all-threads)))


(defun find-by-id (thread-id)
    (reduce (lambda (acc thread)
                (or acc
                    (when (equalp thread-id (get-thread-id thread))
                          thread)))
            (bt:all-threads)
        :initial-value nil))


(defun kill (thread-id)
    (let ((thread (find-by-id thread-id)))

        (unless thread
            (error (make-condition 'thread-not-found :id thread-id)))

        (bt:destroy-thread thread)))
