(defpackage :alive/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
    (:local-nicknames (:astreams :alive/sys/streams)))

(in-package :alive/file)


(declaim (ftype (function (symbol string) *) do-cmd))
(defun do-cmd (cmd path)
    (astreams:with-redirect-streams ()
        (funcall cmd path)))


(declaim (ftype (function (string) *) do-compile))
(defun do-compile (path)
    (do-cmd #+sbcl 'alive/sbcl/file:do-compile
            path))


(declaim (ftype (function (string) *) do-load))
(defun do-load (path)
    (do-cmd #+sbcl 'alive/sbcl/file:do-load
            path))


(declaim (ftype (function (string) *) try-compile))
(defun try-compile (path)
    (do-cmd #+sbcl 'alive/sbcl/file:try-compile
            path))
