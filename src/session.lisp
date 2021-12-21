(defpackage :alive/session
    (:use :cl)
    (:export :start))

(in-package :alive/session)


; (msg (parse:from-stream (usocket:socket-stream conn)))

(defclass client-session ()
    ((conn :accessor conn
           :initform nil
           :initarg :conn)))


(defun start (conn)
    (let ((session (make-instance 'client-session :conn conn)))
        (format T "start session ~A~%" session)))
