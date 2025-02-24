(defpackage :alive/session/transport
    (:use :cl)
    (:export :read-msg
             :send-msg
             :send-request)
    (:local-nicknames (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:state :alive/session/state)))

(in-package :alive/session/transport)


(declaim (ftype (function (T) (or null cons)) read-msg))
(defun read-msg (input-stream)
    (parse:from-stream input-stream))


(declaim (ftype (function (state:state T T)) send-msg))
(defun send-msg (state out-stream msg)
    (state:lock (state mutex)
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) out-stream)
              (force-output out-stream))))


(declaim (ftype (function (state:state T hash-table) cons) send-request))
(defun send-request (state out-stream req)
    (state:lock (state mutex)
        (let ((cond-var (bt:make-condition-variable))
              (response nil))
            (state:set-sent-msg-callback state (gethash "id" req)
                                         (lambda (resp)
                                             (state:lock (state mutex)
                                                 (setf response resp)
                                                 (bt:condition-notify cond-var))))
            (send-msg state out-stream req)
            (unless response
                (bt:condition-wait cond-var mutex))

            response)))
