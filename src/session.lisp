(defpackage :alive/session
    (:use :cl)
    (:export :start)
    (:local-nicknames (:parse :alive/lsp/parse)
                      (:message :alive/lsp/message)
                      (:init-req :alive/lsp/init-request)
                      (:init-res :alive/lsp/init-response)))

(in-package :alive/session)


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


(defgeneric handle-msg (session msg))
(defgeneric handle-req (session msg req))


(defmethod handle-msg (session (msg message:request-payload))
    (format T "Handle request ~A~%" msg)
    (handle-req session msg (message:params msg)))


(defmethod handle-req (session (msg message:request-payload) (req init-req::params))
    (let* ((result (init-res:create))
           (resp-msg (message:create-response (message:id msg) result)))
        (format T "Handle init request: ~A~%" (message:to-wire resp-msg))))


(defun read-message (session)
    (usocket:wait-for-input (conn session))

    (let ((in-stream (usocket:socket-stream (conn session))))
        (when (listen in-stream)
              (parse:from-stream in-stream))))


(defun read-messages (session)
    (let ((msg (read-message session)))
        (format T "MSG ~A~%" (message::method-name msg))
        (handle-msg session msg)))


(defun start-read-thread (session)
    (let ((stdout *standard-output*))
        (setf (read-thread session)
              (bt:make-thread (lambda ()
                                  (let ((*standard-output* stdout))
                                      (read-messages session)))
                              :name "Session Message Reader"))))


(defun start (conn)
    (let* ((session (make-instance 'client-session :conn conn)))
        (format T "start session ~A~%" session)
        (start-read-thread session)))
