(defpackage :alive/errors
    (:use :cl)
    (:export :input-error
             :start
             :end))

(in-package :alive/errors)


(define-condition input-error (error)
        ((start :accessor start
                :initform nil
                :initarg :start)
         (end :accessor end
              :initform nil
              :initarg :end)
         (message :accessor message
                  :initform nil
                  :initarg :message))

    (:report (lambda (condition stream)
                 (format stream "[~A:~A] ~A"
                     (start condition)
                     (end condition)
                     (message condition)))))
