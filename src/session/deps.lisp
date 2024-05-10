(defpackage :alive/session/deps
    (:use :cl)
    (:export :create
             :deps
             :msg-handler
             :read-msg
             :send-msg
             :with-deps))

(in-package :alive/session/deps)


(defparameter *deps* nil)


(defstruct deps
    (msg-handler nil)
    (read-msg nil)
    (send-msg nil))


(declaim (ftype (function (&key (:msg-handler (function (cons) (values (or null hash-table) &optional)))
                                (:send-msg (function (cons) null))
                                (:read-msg (function () (values (or null cons) &optional))))
                          deps) create))
(defun create (&key msg-handler send-msg read-msg)
    (make-deps :msg-handler msg-handler
               :send-msg send-msg
               :read-msg read-msg))


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


(defmacro with-deps (deps &body body)
    `(let ((*deps* ,deps))
         ,@body))
