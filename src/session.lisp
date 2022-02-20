(defpackage :alive/session
    (:use :cl)
    (:export :add-listener
             :listener
             :start
             :stop)
    (:local-nicknames (:did-open :alive/lsp/message/document/did-open)
                      (:did-change :alive/lsp/message/document/did-change)
                      (:init :alive/lsp/message/initialize)
                      (:logger :alive/logger)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)))

(in-package :alive/session)


(defclass listener ()
    ((on-done :accessor on-done
              :initform nil
              :initarg :on-done)))


(defclass client-session ()
    ((running :accessor running
              :initform T
              :initarg :running)
     (listeners :accessor listeners
                :initform nil
                :initarg :listeners)
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


(defmethod add-listener ((obj client-session) (to-add listener))
    (push to-add (listeners obj)))


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
    (let ((uri (did-open:get-uri msg))
          (text (did-open:get-text msg)))

        (when text
              (setf (gethash uri (files session)) text))))


(defmethod handle-msg (session (msg did-change:did-change))
    (let ((uri (did-change:get-uri msg))
          (text (did-change:get-text msg)))

        (when text
              (setf (gethash uri (files session)) text))))


(defmethod handle-msg (session (msg sem-tokens:request))
    (let* ((params (message:params msg))
           (doc (sem-tokens:text-document params))
           (uri (text-doc:uri doc))
           (text (gethash uri (files session))))

        (when text
              (send-msg session (sem-tokens:create-response (message:id msg) text)))))


(defun read-message (session)
    (handler-case
            (let ((in-stream (usocket:socket-stream (conn session))))
                (parse:from-stream in-stream))
        (end-of-file (c)
                     (declare (ignore c))
                     (logger:error-msg (logger session) "EOF caught, assuming socket is closed")
                     (stop session))
        (error (c)
               (logger:error-msg (logger session) "read-message: ~A" c))))


(defun read-messages (session)
    (loop :while (running session)
          :do (let ((msg (read-message session)))
                  (logger:debug-msg (logger session) "MSG ~A" (json:encode-json-to-string msg))
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
        (logger:info-msg logger "Started session ~A" session)
        session))


(defun stop (session)
    (logger:info-msg (logger session) "Stopping session ~A" session)
    (setf (running session) nil)

    (when (conn session)
          (usocket:socket-close (conn session))
          (setf (conn session) nil))

    (loop :for listener :in (listeners session) :do
              (when (on-done listener)
                    (funcall (on-done listener)))))
