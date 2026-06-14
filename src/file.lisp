(defpackage :alive/file
    (:use :cl)
    (:export :xyz-do-compile
             :xyz-do-load
             :xyz-try-compile)
    (:local-nicknames (:astreams :alive/sys/streams)))

(in-package :alive/file)


(declaim (ftype (function (symbol string &key (:stdin-fn (or null function)) (:stdout-fn (or null function)) (:stderr-fn (or null function))) *) do-cmd))
(defun do-cmd (cmd path &key stdin-fn stdout-fn stderr-fn)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)
        (funcall cmd path)))


(declaim (ftype (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) xyz-do-compile))
(defun xyz-do-compile (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:xyz-do-compile path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(declaim (ftype (function (string &key (:stdin-fn function) (:stdout-fn function) (:stderr-fn function)) *) xyz-do-load))
(defun xyz-do-load (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:xyz-do-load path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(declaim (ftype (function (string) *) xyz-try-compile))
(defun xyz-try-compile (path)
    (do-cmd #+sbcl 'alive/sbcl/file:xyz-try-compile
            path))
