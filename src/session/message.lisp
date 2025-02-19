(defpackage :alive/session/message
    (:use :cl)
    (:export :handle
             :new-handle)
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


(declaim (ftype (function (handlers:list-of-handlers cons) (values (or null hash-table) &optional)) new-handle-request))
(defun new-handle-request (handlers msg)
    (let* ((method-name (cdr (assoc :method msg)))
           (handler (handlers:new-get-handler handlers method-name)))

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
          (T (lsp-msg:create-error (cdr (assoc :id msg))
                                   :code errors:*request-failed*
                                   :message (format nil "No handler for message")))))


(declaim (ftype (function (handlers:list-of-handlers cons) (values (or null hash-table) &optional)) new-handle))
(defun new-handle (handlers msg)
    (cond ((assoc :method msg) (new-handle-request handlers msg))
          ((or (assoc :result msg)
               (assoc :error msg)) (handle-response msg))
          (T (lsp-msg:create-error (cdr (assoc :id msg))
                                   :code errors:*request-failed*
                                   :message (format nil "No handler for message")))))
