(defpackage :alive/types
    (:use :cl)
    (:export :make-compile-message
             :*sev-error*
             :*sev-warn*
             :*sev-info*

             :*comment*
             :*string*
             :*keyword*
             :*number*
             :*namespace*
             :*function*
             :*macro*
             :*variable*
             :*parameter*
             :*open-paren*
             :*close-paren*
             :*symbol*
             :*ws*
             :*colons*
             :*ifdef-true*
             :*ifdef-false*

             :deep-equal-p))

(in-package :alive/types)


(defparameter *sev-error* "error")
(defparameter *sev-warn* "warning")
(defparameter *sev-info* "info")


(defparameter *comment* 0)
(defparameter *string* 1)
(defparameter *keyword* 2)
(defparameter *number* 3)
(defparameter *namespace* 4)
(defparameter *function* 5)
(defparameter *macro* 6)
(defparameter *variable* 7)
(defparameter *parameter* 8)
(defparameter *open-paren* 9)
(defparameter *close-paren* 10)
(defparameter *symbol* 11)
(defparameter *ws* 12)
(defparameter *colons* 13)
(defparameter *ifdef-true* 14)
(defparameter *ifdef-false* 15)


(defstruct compile-message
    severity
    location
    message)


(defgeneric deep-equal-p (a b))
