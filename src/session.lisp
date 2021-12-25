(defpackage :alive/session
    (:use :cl)
    (:export :start
             :stop)
    (:local-nicknames (:did-open :alive/lsp/message/document/did-open)
                      (:init :alive/lsp/message/initialize)
                      (:logger :alive/logger)
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
     (files :accessor files
            :initform (make-hash-table :test 'equalp)
            :initarg :files)
     (logger :accessor logger
             :initform nil
             :initarg :logger)
     (initialized :accessor initialized
                  :initform nil
                  :initarg :initialized)
     (read-thread :accessor read-thread
                  :initform nil
                  :initarg :read-thread)))


(defgeneric handle-msg (session msg))


(defun send-msg (session msg)
    (logger:trace-msg (logger session) "<-- ~A~%" (json:encode-json-to-string msg))

    (let ((to-send (packet:to-wire msg)))
        (write-string to-send (usocket:socket-stream (conn session)))
        (force-output (usocket:socket-stream (conn session)))))


(defmethod handle-msg (session (msg init:request))
    (let* ((resp (init:create-response (message:id msg))))
        (send-msg session resp)))


(defmethod handle-msg (session (msg init:initialized))
    (setf (initialized session) T))


(defmethod handle-msg (session (msg did-open:did-open))
    (let ((key (did-open:get-uri msg))
          (value (did-open:get-text msg)))
        (setf (gethash key (files session)) value)))


(defun read-message (session)
    (handler-case
            (let ((in-stream (usocket:socket-stream (conn session))))
                (parse:from-stream in-stream))
        (error (c)
               (logger:error-msg (logger session) "~A" c)
               (stop session))))


; (defun read-message (session)
;     (usocket:wait-for-input (conn session) :ready-only T)
;     (format T "WOKE UP~%")

;     (handler-case
;             (let ((in-stream (usocket:socket-stream (conn session))))
;                 (when (listen in-stream)
;                       (parse:from-stream in-stream)))
;         (error (c) (logger:error-msg (logger session) "~A" c))))


(defun read-messages (session)
    (loop :while (running session)
          :do (let ((msg (read-message session)))
                  (logger:debug-msg (logger session) "MSG ~A" msg)
                  (when msg
                        (logger:trace-msg (logger session) "--> ~A~%" (json:encode-json-to-string msg))
                        (handle-msg session msg)))))


(defun start-read-thread (session)
    (let ((stdout *standard-output*))
        (setf (read-thread session)
              (bt:make-thread (lambda ()
                                  (let ((*standard-output* stdout))
                                      (read-messages session)))
                              :name "Session Message Reader"))))

(defun start (logger conn)
    (let* ((session (make-instance 'client-session
                                   :conn conn
                                   :Logger logger)))
        (start-read-thread session)
        session))


(defun stop (session)
    (when (conn session)
          (usocket:socket-close (conn session))
          (setf (conn session) nil)))
