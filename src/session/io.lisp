(defpackage :alive/session/io
    (:use :cl)
    (:export :read-message
             :send-msg)
    (:local-nicknames (:context :alive/context)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/parse/forms)
                      (:state :alive/session/state)))

(in-package :alive/session/io)


(declaim (ftype (function () (or null cons)) read-message))
(defun read-message ()
    (parse:from-stream (context:get-input-stream)))


(declaim (ftype (function (state:state T)) send-msg))
(defun send-msg (state msg)
    (bt:with-recursive-lock-held ((state:lock state))
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (packet:to-wire msg) (context:get-output-stream))
              (force-output (context:get-output-stream)))))
