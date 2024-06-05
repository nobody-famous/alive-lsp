(defpackage :alive/session/transport
    (:use :cl)
    (:export :read-msg
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


(declaim (ftype (function (T)) send-msg))
(defun send-msg (msg)
    (state:lock (mutex)
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) (context:get-output-stream))
              (force-output (context:get-output-stream)))))


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
