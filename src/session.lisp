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
           (resp-msg (message:create-result (message:id msg) result))
           (to-send (message:to-wire resp-msg)))
        (format T "Handle init request: ~A~%" to-send)
        (write to-send :stream (usocket:socket-stream (conn session)))))


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
        (format T "start session ~A~%" session)
        (start-read-thread session)))
