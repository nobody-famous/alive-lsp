(defpackage :alive/session/message-loop
    (:use :cl)
    (:export :new-run
             :run
             :stop)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/message-loop)


(declaim (ftype (function (cons) (values (or null hash-table) &optional)) process-msg))
(defun process-msg (msg)
    (let ((id (cdr (assoc :id msg))))
        (state:with-thread-msg (id)
            (handler-case
                    (funcall (deps:msg-handler) msg)
                (error (c)
                    (logger:error-msg "Message Handler: ~A ~A" msg c)
                    (lsp-msg:create-error id
                                          :code errors:*internal-error*
                                          :message (princ-to-string c)))))))


(declaim (ftype (function (deps:dependencies cons) (values (or null hash-table) &optional)) new-process-msg))
(defun new-process-msg (deps msg)
    (let ((id (cdr (assoc :id msg))))
        (state:new-with-thread-msg (deps id)
            (handler-case
                    (funcall (deps:new-msg-handler deps) deps msg)
                (error (c)
                    (logger:error-msg "Message Handler: ~A ~A" msg c)
                    (lsp-msg:create-error id
                                          :code errors:*internal-error*
                                          :message (princ-to-string c)))))))


(declaim (ftype (function () null) stop))
(defun stop ()
    (logger:info-msg "Stopping message loop")
    (state:set-running NIL)
    nil)


(declaim (ftype (function () (values (or null hash-table) &optional)) get-next-response))
(defun get-next-response ()
    (handler-case
            (let ((msg (deps:read-msg)))
                (when msg
                      (process-msg msg)))

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
                     (stop))

        (T (c)
           (logger:error-msg "Unknown Error: ~A" (type-of c))
           (stop))))


(declaim (ftype (function (deps:dependencies) (values (or null hash-table) &optional)) new-get-next-response))
(defun new-get-next-response (deps)
    (handler-case
            (let ((msg (deps:new-read-msg deps)))
                (when msg
                      (new-process-msg deps msg)))

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
                     (stop))

        (T (c)
           (logger:error-msg "Unknown Error: ~A ~A" (type-of c) c)
           (stop))))


(declaim (ftype (function () null) run))
(defun run ()
    (state:set-running T)
    (loop :while (state:running)
          :do (let ((resp (get-next-response)))
                  (when resp
                        (deps:send-msg resp)))))


(declaim (ftype (function (deps:dependencies) null) new-run))
(defun new-run (deps)
    (state:set-running T)
    (loop :while (state:running)
          :do (let ((resp (new-get-next-response deps)))
                  (when resp
                        (deps:new-send-msg deps resp)))))
