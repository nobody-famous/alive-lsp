(defpackage :alive/session/io
    (:use :cl)
    (:export :read-message
             :send-msg)
    (:local-nicknames (:context :alive/context)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:state :alive/session/state)))

(in-package :alive/session/io)


(declaim (ftype (function () (or null cons)) read-message))
(defun read-message ()
    (parse:from-stream (context:get-input-stream)))


(declaim (ftype (function (T)) send-msg))
(defun send-msg (msg)
    (bt:with-recursive-lock-held ((state:lock))
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) (context:get-output-stream))
              (force-output (context:get-output-stream)))))
