(defpackage :alive/sbcl/threads
    (:use :cl)
    (:export :get-thread-id))

(in-package :alive/sbcl/threads)


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
