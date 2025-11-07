(defpackage :alive/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
    (:local-nicknames (:astreams :alive/sys/streams)))

(in-package :alive/file)


(declaim (ftype (function (symbol string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) do-cmd))
(defun do-cmd (cmd path &key stdin-fn stdout-fn stderr-fn)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)
        (funcall cmd path)))


(declaim (ftype (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) do-compile))
(defun do-compile (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:do-compile
            path
            :stdin-fn stdin-fn
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))


(declaim (ftype (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) do-load))
(defun do-load (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:do-load
            path
            :stdin-fn stdin-fn
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))


(declaim (ftype (function (string &key (:stdin-fn (or function null)) (:stdout-fn function) (:stderr-fn function)) *) try-compile))
(defun try-compile (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:try-compile
            path
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))
