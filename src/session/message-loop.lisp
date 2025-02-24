(defpackage :alive/session/message-loop
    (:use :cl)
    (:export :run
             :stop)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/message-loop)


(declaim (ftype (function (deps:dependencies state:state cons) (values (or null hash-table) &optional)) process-msg))
(defun process-msg (deps state msg)
    (let ((id (cdr (assoc :id msg))))
        (state:with-thread-msg (state deps id)
            (handler-case
                    (funcall (deps:msg-handler deps) deps msg)
                (error (c)
                    (logger:error-msg "Message Handler: ~A ~A" msg c)
                    (lsp-msg:create-error id
                                          :code errors:*internal-error*
                                          :message (princ-to-string c)))))))


(declaim (ftype (function (state:state) null) stop))
(defun stop (state)
    (logger:info-msg "New Stopping message loop")
    (state:set-running state NIL)
    nil)


(declaim (ftype (function (deps:dependencies state:state) (values (or null hash-table) &optional)) get-next-response))
(defun get-next-response (deps state)
    (handler-case
            (let ((msg (deps:read-msg deps)))
                (when msg
                      (process-msg deps state msg)))

        (errors:unhandled-request (c)
                                  (logger:error-msg "Unhandled Request: ~A" c)
                                  (when (errors:id c)
                                        (lsp-msg:create-error (errors:id c)
                                                              :code errors:*method-not-found*
                                                              :message (format nil "Unhandled request: ~A" (errors:method-name c)))))

        (errors:server-error (c)
                             (logger:error-msg "Server Error: ~A" c)
                             (when (errors:id c)
                                   (lsp-msg:create-error (errors:id c)
                                                         :code errors:*internal-error*
                                                         :message (format nil "Server error: ~A" (errors:message c)))))

        (alive/session/spawn:spawned-thread (c) (declare (ignore c)))

        (end-of-file (c)
                     (declare (ignore c))
                     (stop state))

        (T (c)
           (logger:error-msg "Unknown Error: ~A ~A" (type-of c) c)
           (stop state))))


(declaim (ftype (function (deps:dependencies state:state) null) run))
(defun run (deps state)
    (loop :while (state:running state)
          :do (let ((resp (get-next-response deps state)))
                  (when resp
                        (deps:send-msg deps resp)))))
