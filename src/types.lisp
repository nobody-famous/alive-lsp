(defpackage :alive/types
    (:use :cl)
    (:export :*sev-error*
             :*sev-warn*
             :*sev-info*

             :*line-comment*
             :*block-comment*
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
             :*quote*
             :*back-quote*
             :*comma*
             :*comma-at*

             :deep-equal-p))

(in-package :alive/types)


(defparameter *sev-error* "error")
(defparameter *sev-warn* "warning")
(defparameter *sev-info* "info")


(defparameter *line-comment* 0)
(defparameter *block-comment* 1)
(defparameter *string* 2)
(defparameter *keyword* 3)
(defparameter *number* 4)
(defparameter *namespace* 5)
(defparameter *function* 6)
(defparameter *macro* 7)
(defparameter *variable* 8)
(defparameter *parameter* 9)
(defparameter *open-paren* 10)
(defparameter *close-paren* 11)
(defparameter *symbol* 12)
(defparameter *ws* 13)
(defparameter *colons* 14)
(defparameter *ifdef-true* 15)
(defparameter *ifdef-false* 16)
(defparameter *quote* 17)
(defparameter *back-quote* 18)
(defparameter *comma* 19)
(defparameter *comma-at* 20)


(defgeneric deep-equal-p (a b))
