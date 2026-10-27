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
                    (logger:error-msg (state:get-log state) "Message Handler: ~A ~A" msg c)
                    (lsp-msg:create-error id
                                          :code errors:*internal-error*
                                          :message (princ-to-string c)))))))


(declaim (ftype (function (state:state) null) stop))
(defun stop (state)
    (logger:info-msg (state:get-log state) "Stopping message loop")
    (state:set-running state NIL)
    nil)


(declaim (ftype (function (deps:dependencies state:state) (values (or null hash-table) &optional)) get-next-response))
(defun get-next-response (deps state)
    (let ((msg (deps:read-msg deps)))
        (when msg
              (process-msg deps state msg))))


(declaim (ftype (function (deps:dependencies state:state) null) run))
(defun run (deps state)
    (loop :while (state:running state)
          :do (let ((resp (get-next-response deps state)))
                  (when resp
                        (deps:send-msg deps resp)))))
