(defpackage :alive/deps
    (:use :cl)
    (:export :create
             :deps
             :do-eval
             :get-thread-id
             :kill-thread
             :list-all-threads
             :list-all-asdf
             :msg-handler
             :read-msg
             :send-msg
             :send-request
             :with-deps))

(in-package :alive/deps)


(defparameter *deps* nil)


(defstruct deps
    (msg-handler nil :type (or null (function (cons) (values (or null hash-table) &optional))))
    (read-msg nil :type (or null (function () (values (or null cons) &optional))))
    (send-msg nil :type (or null (function (cons) null)))
    (send-request nil :type (or null (function (cons) hash-table)))
    (eval-fn nil :type (or null (function (T) *)))
    (list-all-threads nil :type (or null (function () cons)))
    (kill-thread nil :type (or null (function (T) *)))
    (list-all-asdf nil :type (or null (function () cons)))
    (get-thread-id nil :type (or null (function (bt:thread) *))))


(declaim (ftype (function (&key (:msg-handler (function (cons) (values (or null hash-table) &optional)))
                                (:send-msg (function (cons) null))
                                (:send-request (function (hash-table) cons))
                                (:read-msg (function () (values (or null cons) &optional)))
                                (:list-all-threads (function () cons))
                                (:kill-thread (function (T) *))
                                (:list-all-asdf (function () cons))
                                (:get-thread-id (function (bt:thread) *))
                                (:eval-fn (function (stream) *)))
                          deps) create))
(defun create (&key msg-handler send-msg send-request read-msg list-all-threads kill-thread list-all-asdf get-thread-id eval-fn)
    (make-deps :msg-handler msg-handler
               :send-msg send-msg
               :send-request send-request
               :read-msg read-msg
               :list-all-threads list-all-threads
               :kill-thread kill-thread
               :list-all-asdf list-all-asdf
               :get-thread-id get-thread-id
               :eval-fn eval-fn))


(declaim (ftype (function () T) msg-handler))
(defun msg-handler ()
    (unless *deps* (error "Dependencies not set"))
    (deps-msg-handler *deps*))


(declaim (ftype (function () T) read-msg))
(defun read-msg ()
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-read-msg *deps*) (error "Dependencies read-msg not set"))

    (funcall (deps-read-msg *deps*)))


(declaim (ftype (function (T) (values null &optional)) send-msg))
(defun send-msg (msg)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-send-msg *deps*) (error "Dependencies send-msg not set"))

    (funcall (deps-send-msg *deps*) msg))


(declaim (ftype (function (hash-table) (values cons &optional)) send-request))
(defun send-request (msg)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-send-request *deps*) (error "Dependencies send-request not set"))

    (funcall (deps-send-request *deps*) msg))


(declaim (ftype (function () (values cons &optional)) list-all-threads))
(defun list-all-threads ()
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-list-all-threads *deps*) (error "Dependencies list-all-threads not set"))

    (funcall (deps-list-all-threads *deps*)))


(declaim (ftype (function (T) *) kill-thread))
(defun kill-thread (thread-id)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-kill-thread *deps*) (error "Dependencies kill-thread not set"))

    (funcall (deps-kill-thread *deps*) thread-id))


(declaim (ftype (function (bt:thread) *) get-thread-id))
(defun get-thread-id (thread)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-get-thread-id *deps*) (error "Dependencies get-thread-id not set"))

    (funcall (deps-get-thread-id *deps*) thread))


(declaim (ftype (function () (values cons &optional)) list-all-asdf))
(defun list-all-asdf ()
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-list-all-asdf *deps*) (error "Dependencies list-all-asdf not set"))

    (funcall (deps-list-all-asdf *deps*)))


(declaim (ftype (function (T) *) do-eval))
(defun do-eval (data)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-eval-fn *deps*) (error "Dependencies eval-fn not set"))

    (funcall (deps-eval-fn *deps*) data))


(defmacro with-deps (deps &body body)
    `(let ((*deps* ,deps))
         (progn ,@body)))
