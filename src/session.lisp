(defpackage :alive/session
    (:use :cl)
    (:export :start))

(in-package :alive/session)


; (msg (parse:from-stream (usocket:socket-stream conn)))

(defclass client-session ()
    ((running :accessor running
              :initform T
              :initarg :running)
     (conn :accessor conn
           :initform nil
           :initarg :conn)
     (read-thread :accessor read-thread
                  :initform nil
                  :initarg :read-thread)))


(defun read-message (session)
    (let* ((inputs (usocket:wait-for-input (list (conn session)))))
        (loop :for ready :in inputs :do
                  (when (listen (usocket:socket-stream (conn session)))
                        nil))))


(defun read-messages (session)
    (loop :while (running session)
          :do (read-message session)))


(defun start (conn)
    (let* ((stdout *standard-output*)
           (session (make-instance 'client-session :conn conn))
           (read-thread (bt:make-thread (lambda ()
                                            (let ((*standard-output* stdout))
                                                (read-messages session))))))
        (setf (read-thread session) read-thread)
        (format T "start session ~A~%" session)))
