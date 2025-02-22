(defpackage :alive/session/transport
    (:use :cl)
    (:export :new-read-msg
             :new-send-msg
             :new-send-request
             :read-msg
             :send-msg
             :send-request)
    (:local-nicknames (:context :alive/context)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:state :alive/session/state)))

(in-package :alive/session/transport)


(declaim (ftype (function () (or null cons)) read-msg))
(defun read-msg ()
    (parse:from-stream (context:get-input-stream)))


(declaim (ftype (function (T) (or null cons)) new-read-msg))
(defun new-read-msg (input-stream)
    (parse:from-stream input-stream))


(declaim (ftype (function (T)) send-msg))
(defun send-msg (msg)
    (state:lock (mutex)
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))

              (let ((steam (context:get-output-stream)))
                  (write-sequence (packet:to-wire msg) steam)
                  (force-output steam)))))


(declaim (ftype (function (state:state T T)) new-send-msg))
(defun new-send-msg (state out-stream msg)
    (state:new-lock (state mutex)
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) out-stream)
              (force-output out-stream))))


(declaim (ftype (function (hash-table) cons) send-request))
(defun send-request (req)
    (state:lock (mutex)
        (let ((cond-var (bt:make-condition-variable))
              (response nil))
            (state:set-sent-msg-callback (gethash "id" req)
                                         (lambda (resp)
                                             (state:lock (mutex)
                                                 (setf response resp)
                                                 (bt:condition-notify cond-var))))
            (send-msg req)
            (unless response
                (bt:condition-wait cond-var mutex))

            response)))


(declaim (ftype (function (state:state T hash-table) cons) new-send-request))
(defun new-send-request (state out-stream req)
    (state:new-lock (state mutex)
        (let ((cond-var (bt:make-condition-variable))
              (response nil))
            (state:set-sent-msg-callback (gethash "id" req)
                                         (lambda (resp)
                                             (state:new-lock (state mutex)
                                                 (setf response resp)
                                                 (bt:condition-notify cond-var))))
            (new-send-msg state out-stream req)
            (unless response
                (bt:condition-wait cond-var mutex))

            response)))
