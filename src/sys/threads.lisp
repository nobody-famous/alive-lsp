(defpackage :alive/sys/threads
    (:use :cl)
    (:export :get-id
             :kill
             :list-all))

(in-package :alive/sys/threads)


(defun get-id (thread)
    #+sbcl (alive/sbcl/threads:get-thread-id thread))


(defun thread-to-wire (thread)
    (let ((table (make-hash-table :test #'equalp)))
        (setf (gethash "id" table) (get-id thread))
        (setf (gethash "name" table) (get-id thread))
        table))


(defun list-all ()
    (mapcar #'thread-to-wire (bt:all-threads)))


(defun find-by-id (thread-id)
    (find-if (lambda (thread) (equalp thread-id (get-id thread)))
            (bt:all-threads)))


(defun kill (thread-id)
    (let ((thread (find-by-id thread-id)))
        (when thread
              (bt:destroy-thread thread))))
