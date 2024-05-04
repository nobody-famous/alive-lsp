(defpackage :alive/context
    (:use :cl)
    (:export :create
             :destroy
             :get-input-stream
             :get-output-stream
             :with-context))

(in-package :alive/context)


(defparameter *context* nil)


(define-condition context-nil-error (error)
        (#| slots |#)
    (:report (lambda (condition stream)
                 (declare (ignore condition))
                 (format stream "No context has been created"))))


(defstruct context
    (input-stream nil)
    (output-stream nil)
    (destroy nil :type (function ())))


(defun get-input-stream ()
    (unless *context*
        (error (make-condition 'context-nil-error)))

    (context-input-stream *context*))


(defun get-output-stream ()
    (unless *context*
        (error (make-condition 'context-nil-error)))

    (context-output-stream *context*))


(defun destroy ()
    (unless *context*
        (error (make-condition 'context-nil-error)))
    (context-destroy *context*))


(defmacro with-context ((&key input-stream output-stream destroy-fn) &body body)
    `(let ((*context* (make-context :input-stream ,input-stream
                                    :output-stream ,output-stream
                                    :destroy ,destroy-fn)))
         (unwind-protect
                 ,@body
             (when (functionp (context-destroy *context*))
                   (funcall (context-destroy *context*))))))
