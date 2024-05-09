(defpackage :alive/session/message
    (:use :cl)
    (:export :handle)
    (:local-nicknames (:errors :alive/lsp/errors)
                      (:handlers :alive/session/handlers)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:state :alive/session/state)))

(in-package :alive/session/message)


(declaim (ftype (function (cons) (values (or null hash-table) &optional)) handle-request))
(defun handle-request (msg)
    (let* ((method-name (cdr (assoc :method msg)))
           (handler (handlers:get-handler method-name)))

        (if handler
            (funcall handler msg)
            (let ((error-msg (format nil "No handler for ~A" method-name))
                  (id (cdr (assoc :id msg))))
                (logger:error-msg error-msg)
                (when id (lsp-msg:create-error id
                                               :code errors:*request-failed*
                                               :message error-msg))))))


(declaim (ftype (function (cons) (values (or null hash-table) &optional)) handle-response))
(defun handle-response (msg)
    (let* ((msg-id (cdr (assoc :id msg)))
           (cb (state:get-sent-msg-callback msg-id)))

        (if cb
            (funcall cb msg)
            (lsp-msg:create-error msg-id
                                  :code errors:*request-failed*
                                  :message (format nil "No callback for request: ~A" msg-id)))))


(declaim (ftype (function (cons) (values (or null hash-table) &optional)) handle))
(defun handle (msg)
    (cond ((assoc :method msg) (handle-request msg))
          ((or (assoc :result msg)
               (assoc :error msg)) (handle-response msg))
          (T (lsp-msg:create-error (assoc :id msg)
                                   :code errors:*request-failed*
                                   :message (format nil "No handler for message")))))
