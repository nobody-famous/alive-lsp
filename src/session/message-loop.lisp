(defpackage :alive/session/message-loop
    (:use :cl)
    (:export :run)
    (:local-nicknames (:context :alive/context)
                      (:errors :alive/lsp/errors)
                      (:io :alive/session/io)
                      (:logger :alive/logger)
                      (:message :alive/lsp/message/abstract)
                      (:parse :alive/parse/forms)
                      (:state :alive/session/state)))

(in-package :alive/session/message-loop)


(declaim (ftype (function (state:state T)) process-msg))
(defun process-msg (state msg)
    (let ((id (cdr (assoc :id msg))))
        (state:with-thread-msg (state id)
            nil)))


(declaim (ftype (function (state:state)) stop))
(defun stop (state)
    (logger:info-msg "Stopping state ~A" state)

    (state:set-running state NIL)

    (context:destroy))


(declaim (ftype (function (state:state)) get-next-response))
(defun get-next-response (state)
    (handler-case
            (let ((msg (io:read-message)))
                (when msg
                      (process-msg state msg)))

        (errors:unhandled-request (c)
                                  (logger:error-msg "read-message: ~A" c)
                                  (when (errors:id c)
                                        (message:create-error (errors:id c)
                                                              :code errors:*method-not-found*
                                                              :message (format nil "Unhandled request: ~A" (errors:method-name c)))))

        (errors:server-error (c)
                             (logger:error-msg "read-message: ~A" c)
                             (when (errors:id c)
                                   (message:create-error (errors:id c)
                                                         :code errors:*internal-error*
                                                         :message (format nil "Server error: ~A" (errors:message c)))))

        (end-of-file (c)
                     (declare (ignore c))
                     (stop state))

        (T (c)
           (logger:error-msg "read-message: ~A" c)
           (stop state))))


(declaim (ftype (function (state:state) null) run))
(defun run (state)
    (loop :while (state:running state)
          :do (let ((resp (get-next-response state)))
                  (when resp
                        (io:send-msg state resp)))))
