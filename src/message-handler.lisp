(defpackage :alive/message-handler
    (:use :cl)
    (:export :start
             :stop)
    (:local-nicknames (:session :alive/session)
                      (:did-open :alive/lsp/message/document/did-open)
                      (:did-change :alive/lsp/message/document/did-change)
                      (:init :alive/lsp/message/initialize)
                      (:logger :alive/logger)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:errors :alive/lsp/errors)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)))

(in-package :alive/message-handler)


(defclass listener ()
    ((on-done :accessor on-done
              :initform nil
              :initarg :on-done)))


(defclass message-handler ()
    ((running :accessor running
              :initform T
              :initarg :running)
     (listeners :accessor listeners
                :initform nil
                :initarg :listeners)
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

        (errors:unhandled-request (c)
                                  (logger:error-msg (logger session) "read-message: ~A" c)
                                  (send-msg session
                                            (message:create-error-resp :id (errors:id c)
                                                                       :code errors:*method-not-found*
                                                                       :message (format nil "Unhandled request: ~A" (errors:method-name c)))))

        (errors:server-error (c)
                             (logger:error-msg (logger session) "read-message: ~A" c)
                             (send-msg session
                                       (message:create-error-resp :id (errors:id c)
                                                                  :code errors:*internal-error*
                                                                  :message (format nil "Server error: ~A" (errors:message c)))))

        (error (c)
               (logger:error-msg (logger session) "read-message: ~A" c)
               (send-msg session (message:create-error-resp
                                  :id (errors:id c)
                                  :code errors:*internal-error*
                                  :message "Internal Server Error")))))


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


(defun start (session)
    (session:set-started session)

    (start-read-thread session)

    (logger:info-msg (logger session) "Started session ~A" session))


(defun stop (session)
    (logger:info-msg (logger session) "Stopping session ~A" session)

    (session:set-stopped session)

    (loop :for listener :in (listeners session) :do
              (when (on-done listener)
                    (funcall (on-done listener)))))
