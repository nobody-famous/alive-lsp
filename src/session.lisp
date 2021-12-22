(defpackage :alive/session
    (:use :cl)
    (:export :start)
    (:local-nicknames (:parse :alive/lsp/parse)
                      (:types :alive/lsp/types)
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
(defgeneric handle-req (session req))


(defmethod handle-msg (session (msg types:request-payload))
    (format T "Handle request ~A~%" msg)
    (handle-req session (types:params msg)))


(defmethod handle-req (session (req init-req::params))
    (let* ((resp (init-res:create))
           (resp-json (json:encode-json-to-string resp)))
        (format T "Handle init request: ~A~%" resp-json)))


(defun read-message (session)
    (usocket:wait-for-input (conn session))

    (let ((in-stream (usocket:socket-stream (conn session))))
        (when (listen in-stream)
              (parse:from-stream in-stream))))


(defun read-messages (session)
    (let ((msg (read-message session)))
        (format T "MSG ~A~%" (types::method-name msg))
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
