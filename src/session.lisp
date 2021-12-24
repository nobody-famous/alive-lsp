(defpackage :alive/session
    (:use :cl)
    (:export :start
             :stop)
    (:local-nicknames (:init :alive/lsp/message/initialize)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)))

(in-package :alive/session)


(defclass client-session ()
    ((running :accessor running
              :initform T
              :initarg :running)
     (conn :accessor conn
           :initform nil
           :initarg :conn)
     (initialized :accessor initialized
                  :initform nil
                  :initarg :initialized)
     (read-thread :accessor read-thread
                  :initform nil
                  :initarg :read-thread)))


(defgeneric handle-msg (session msg))


(defmethod handle-msg (session (msg init:request))
    (format T "Handle init request~%")

    (let* ((resp (init:create-response (message:id msg)))
           (to-send (packet:to-wire resp)))
        (write-string to-send (usocket:socket-stream (conn session)))
        (force-output (usocket:socket-stream (conn session)))))


(defmethod handle-msg (session (msg init:initialized))
    (setf (initialized session) T))


(defun read-message (session)
    (usocket:wait-for-input (conn session))

    (let ((in-stream (usocket:socket-stream (conn session))))
        (when (listen in-stream)
              (parse:from-stream in-stream))))


(defun read-messages (session)
    (loop :while (running session)
          :do (let ((msg (read-message session)))
                  (when msg (handle-msg session msg)))))


(defun start-read-thread (session)
    (let ((stdout *standard-output*))
        (setf (read-thread session)
              (bt:make-thread (lambda ()
                                  (let ((*standard-output* stdout))
                                      (read-messages session)))
                              :name "Session Message Reader"))))


(defun start (conn)
    (let* ((session (make-instance 'client-session :conn conn)))
        (start-read-thread session)
        session))


(defun stop (session)
    (when (conn session)
          (usocket:socket-close (conn session))
          (setf (conn session) nil)))