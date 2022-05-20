(defpackage :alive/debugger
    (:use :cl)
    (:export :for-cond)
    (:local-nicknames (:threads :alive/threads)))

(in-package :alive/debugger)


(defclass debugger ()
        ((message :accessor message
                  :initform nil
                  :initarg :message)
         (restarts :accessor restarts
                   :initform nil
                   :initarg :restarts)
         (stack-trace :accessor stack-trace
                      :initform nil
                      :initarg :stack-trace)))


(defmethod print-object ((obj debugger) out)
    (format out "{message: ~A; restarts: ~A; stack: ~A}"
        (message obj)
        (restarts obj)
        (stack-trace obj)))


(defun for-cond (c)
    (make-instance 'debugger
        :message (format nil "~A" c)
        :restarts (compute-restarts c)
        :stack-trace (mapcar (lambda (item)
                                 (format nil "~A" item)) (threads:get-stack-trace))))
