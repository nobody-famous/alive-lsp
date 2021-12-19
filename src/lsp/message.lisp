(defpackage :alive/lsp/message
    (:use :cl)
    (:export :header-content-length
             :make-header
    ))

(in-package :alive/lsp/message)


(defstruct header
    (content-length 0)
)


(defstruct base-message
    header
    content
)
