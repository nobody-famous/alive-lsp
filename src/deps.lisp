(defpackage :alive/deps
    (:use :cl)
    (:export :create
             :deps
             :do-compile
             :do-eval
             :do-load
             :get-thread-id
             :kill-thread
             :list-all-threads
             :list-all-asdf
             :load-asdf-system
             :msg-handler
             :read-msg
             :send-msg
             :send-request
             :try-compile
             :with-deps))

(in-package :alive/deps)


(defparameter *deps* nil)


(defstruct deps
    (msg-handler nil :type (or null (function (cons) (values (or null hash-table) &optional))))
    (read-msg nil :type (or null (function () (values (or null cons) &optional))))
    (send-msg nil :type (or null (function (cons) null)))
    (send-request nil :type (or null (function (list) hash-table)))
    (eval-fn nil :type (or null (function (T) *)))
    (list-all-threads nil :type (or null (function () cons)))
    (kill-thread nil :type (or null (function (T) *)))
    (list-all-asdf nil :type (or null (function () cons)))
    (load-asdf-system nil :type (or null (function (&key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) boolean)))
    (get-thread-id nil :type (or null (function (bt:thread) *)))
    (try-compile nil :type (or null (function (string) *)))
    (do-compile nil :type (or null (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *)))
    (do-load nil :type (or null (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *))))


(declaim (ftype (function (&key (:msg-handler (function (cons) (values (or null hash-table) &optional)))
                                (:send-msg (function (cons) null))
                                (:send-request (function (hash-table) list))
                                (:read-msg (function () (values (or null cons) &optional)))
                                (:list-all-threads (function () cons))
                                (:kill-thread (function (T) *))
                                (:list-all-asdf (function () cons))
                                (:load-asdf-system (function (&key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) boolean))
                                (:get-thread-id (function (bt:thread) *))
                                (:eval-fn (function (stream) *))
                                (:try-compile (function (string) *))
                                (:do-compile (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *))
                                (:do-load (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *)))
                          deps) create))
(defun create (&key msg-handler
                    (send-msg (lambda (msg) (declare (ignore msg))))
                    (send-request (lambda (req) (declare (ignore req) (list))))
                    read-msg
                    list-all-threads
                    kill-thread
                    list-all-asdf
                    (load-asdf-system (lambda (&key name stdin-fn stdout-fn stderr-fn force)
                                          (declare (ignore name stdin-fn stdout-fn stderr-fn force))
                                          T))
                    get-thread-id
                    (eval-fn (lambda (s) (declare (ignore s))))
                    (try-compile (lambda (path) (declare (ignore path))))
                    (do-compile (lambda (path &key stdin-fn stdout-fn stderr-fn)
                                    (declare (ignore path stdin-fn stdout-fn stderr-fn))))
                    (do-load (lambda (path &key stdin-fn stdout-fn stderr-fn)
                                 (declare (ignore path stdin-fn stdout-fn stderr-fn)))))
    (make-deps :msg-handler msg-handler
               :send-msg send-msg
               :send-request send-request
               :read-msg read-msg
               :list-all-threads list-all-threads
               :kill-thread kill-thread
               :list-all-asdf list-all-asdf
               :load-asdf-system load-asdf-system
               :get-thread-id get-thread-id
               :eval-fn eval-fn
               :try-compile try-compile
               :do-compile do-compile
               :do-load do-load))


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


(declaim (ftype (function (hash-table) (values list &optional)) send-request))
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


(declaim (ftype (function (&key (:name string) (:stdin-fn function) (:stdout-fn function) (:stderr-fn function) (:force boolean)) (values boolean &optional)) load-asdf-system))
(defun load-asdf-system (&key name stdin-fn stdout-fn stderr-fn force)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-load-asdf-system *deps*) (error "Dependencies load-asdf-system not set"))

    (funcall (deps-load-asdf-system *deps*)
        :name name
        :stdin-fn stdin-fn
        :stdout-fn stdout-fn
        :stderr-fn stderr-fn
        :force force))


(declaim (ftype (function (T) *) do-eval))
(defun do-eval (data)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-eval-fn *deps*) (error "Dependencies eval-fn not set"))

    (funcall (deps-eval-fn *deps*) data))


(declaim (ftype (function (string) *) try-compile))
(defun try-compile (path)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-try-compile *deps*) (error "Dependencies try-compile not set"))

    (funcall (deps-try-compile *deps*) path))


(declaim (ftype (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) do-compile))
(defun do-compile (path &key stdin-fn stdout-fn stderr-fn)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-do-compile *deps*) (error "Dependencies do-compile not set"))

    (funcall (deps-do-compile *deps*) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(declaim (ftype (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) do-load))
(defun do-load (path &key stdin-fn stdout-fn stderr-fn)
    (unless *deps* (error "Dependencies not set"))
    (unless (deps-do-load *deps*) (error "Dependencies do-load not set"))

    (funcall (deps-do-load *deps*) path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(defmacro with-deps (deps &body body)
    `(let ((*deps* ,deps))
         (progn ,@body)))
