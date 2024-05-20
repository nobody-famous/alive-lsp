(defpackage :alive/deps
    (:use :cl)
    (:export :create
             :deps
             :do-eval
             :msg-handler
             :read-msg
             :send-msg
             :with-deps))

(in-package :alive/deps)


(defparameter *deps* nil)


(defstruct deps
    (msg-handler nil :type (or null (function (cons) (values (or null hash-table) &optional))))
    (read-msg nil :type (or null (function () (values (or null cons) &optional))))
    (send-msg nil :type (or null (function (cons) null)))
    (eval-fn nil :type (or null (function (T) *))))


(declaim (ftype (function (&key (:msg-handler (function (cons) (values (or null hash-table) &optional)))
                                (:send-msg (function (cons) null))
                                (:read-msg (function () (values (or null cons) &optional)))
                                (:eval-fn (function (stream) *)))
                          deps) create))
(defun create (&key msg-handler send-msg read-msg eval-fn)
    (make-deps :msg-handler msg-handler
               :send-msg send-msg
               :read-msg read-msg
               :eval-fn eval-fn))


(declaim (ftype (function () T) msg-handler))
(defun msg-handler ()
    (unless *deps* (error "Dependencies not set"))
    (deps-msg-handler *deps*))


(declaim (ftype (function () T) read-msg))
(defun read-msg ()
    (unless *deps* (error "Dependencies not set"))
    (when (deps-read-msg *deps*)
          (funcall (deps-read-msg *deps*))))


(declaim (ftype (function (T) null) send-msg))
(defun send-msg (msg)
    (unless *deps* (error "Dependencies not set"))
    (when (deps-send-msg *deps*)
          (funcall (deps-send-msg *deps*) msg)
          nil))


(declaim (ftype (function (T) *) do-eval))
(defun do-eval (data)
    (unless *deps* (error "Dependencies not set"))
    (funcall (deps-eval-fn data)))


(defmacro with-deps (deps &body body)
    `(let ((*deps* ,deps))
         ,@body))
