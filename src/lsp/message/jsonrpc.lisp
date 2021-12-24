(defpackage :alive/lsp/message/jsonrpc
    (:use :cl)
    (:export :message
             :version))

(in-package :alive/lsp/message/jsonrpc)


(defclass message ()
    ((jsonrpc :accessor version
              :initform "2.0"
              :initarg :jsonrpc)))
