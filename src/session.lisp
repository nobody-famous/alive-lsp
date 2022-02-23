(defpackage :alive/session
    (:use :cl)
    (:export :add-listener
             :create
             :listener
             :start
             :stop)
    (:local-nicknames (:state :alive/state)
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

(in-package :alive/session)


(defclass listener ()
    ((on-done :accessor on-done
              :initform nil
              :initarg :on-done)))


(defclass message-handler ()
    ((logger :accessor logger
             :initform nil
             :initarg :logger)
     (state :accessor state
            :initform nil
            :initarg :state)
     (running :accessor running
              :initform T
              :initarg :running)
     (listeners :accessor listeners
                :initform nil
                :initarg :listeners)
     (read-thread :accessor read-thread
                  :initform nil
                  :initarg :read-thread)))


(defun create (&key logger state)
    (make-instance 'message-handler
                   :logger logger
                   :state state
                   :running nil
                   :listeners nil
                   :read-thread nil))


(defmethod add-listener ((obj message-handler) (to-add listener))
    (push to-add (listeners obj)))


(defgeneric handle-msg (session msg))


(defun send-msg (session msg)
    (logger:trace-msg (logger session) "<-- ~A~%" (json:encode-json-to-string msg))

    (state:send-msg (state session)
                    (packet:to-wire msg)))


(defmethod handle-msg (session (msg init:request))
    (let* ((resp (init:create-response (message:id msg))))
        (send-msg session resp)))


(defmethod handle-msg (session (msg init:initialized))
    (state:set-initialized session T))


(defmethod handle-msg (session (msg did-open:did-open))
    (let ((uri (did-open:get-uri msg))
          (text (did-open:get-text msg)))

        (when text
              (state:set-file-text session uri text))))


(defmethod handle-msg (session (msg did-change:did-change))
    (let ((uri (did-change:get-uri msg))
          (text (did-change:get-text msg)))

        (when text
              (state:set-file-text session uri text))))


(defmethod handle-msg (session (msg sem-tokens:request))
    (let* ((params (message:params msg))
           (doc (sem-tokens:text-document params))
           (uri (text-doc:uri doc))
           (text (state:get-file-text session uri)))

        (when text
              (send-msg session (sem-tokens:create-response (message:id msg) text)))))


(defun read-message (session)
    (handler-case

            (parse:from-stream (state:get-input-stream (state session)))

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
    (setf (running session) T)

    (start-read-thread session)

    (logger:info-msg (logger session) "Started session ~A" session))


(defun stop (session)
    (logger:info-msg (logger session) "Stopping session ~A" session)

    (setf (running session) NIL)

    (state:destroy (state session))

    (loop :for listener :in (listeners session) :do
              (when (on-done listener)
                    (funcall (on-done listener)))))
