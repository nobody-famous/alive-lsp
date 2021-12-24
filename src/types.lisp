(defpackage :alive/types
    (:use :cl)
    (:export :make-compile-message
             :*sev-error*
             :*sev-warn*
             :*sev-info*))

(in-package :alive/types)


(defparameter *sev-error* "error")
(defparameter *sev-warn* "warning")
(defparameter *sev-info* "info")


(defstruct compile-message
    severity
    location
    message)
