(defpackage :alive/sys/threads
    (:use :cl)
    (:export :get-id
             :kill
             :list-all))

(in-package :alive/sys/threads)


(declaim (ftype (function (T) (or null simple-string)) get-id))
(defun get-id (thread)
    #+sbcl (alive/sbcl/threads:get-thread-id thread))


(declaim (ftype (function () cons) list=all))
(defun list-all ()
    (mapcar (lambda (thread)
                (list (cons :id (get-id thread))
                      (cons :name (bt:thread-name thread))))
            (bt:all-threads)))


(declaim (ftype (function (T) bt:thread) find-by-id))
(defun find-by-id (thread-id)
    (find-if (lambda (thread) (equalp thread-id (get-id thread)))
            (bt:all-threads)))


(declaim (ftype (function (T) *) kill))
(defun kill (thread-id)
    (let ((thread (find-by-id thread-id)))
        (when thread
              (bt:destroy-thread thread))))
