(defpackage :alive/lsp/types
    (:use :cl)
    (:export :make-payload
             :make-request-payload
             :make-header
             :header-content-length
             :request-params
    ))

(in-package :alive/lsp/types)


(defstruct header
    content-length
)


(defstruct payload
    jsonrpc
    id
)


(defstruct (request-payload (:include payload))
    method-name
    params
)


(defun request-params (msg)
    (request-payload-params msg)
)