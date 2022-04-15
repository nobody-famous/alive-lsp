(defpackage :alive/threads
    (:use :cl)
    (:export :list-all
             :kill
             :thread-not-found
             :get-thread-id
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


#+win32
(defun get-thread-handle (thread)
    (declare (type sb-thread:thread thread))
    (let* ((pthread-pointer (sb-sys:int-sap (sb-thread::thread-os-thread thread)))
           (pthread-alien (sb-alien:sap-alien
                           pthread-pointer (sb-alien:struct nil
                                                            (start-addr (* t))
                                                            (arg (* t))
                                                            (handle (* t))))))
        (sb-alien:alien-sap (sb-alien:slot pthread-alien 'handle))))


#+win32
(defun get-thread-id (thread)
    (declare (type sb-thread:thread thread))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "GetThreadId" (function sb-alien:unsigned
                                                    (* t)))
     (get-thread-handle thread)))


#-win32
(defun get-thread-id (thread)
    (sxhash thread))


(defun list-all ()
    (mapcar (lambda (thread)
                (make-instance 'thread
                               :id (get-thread-id thread)
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
