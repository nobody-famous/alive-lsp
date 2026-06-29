(defpackage :alive/file
    (:use :cl)
    (:export :xyz-do-compile
             :xyz-do-load
             :xyz-try-compile)
    (:local-nicknames (:astreams :alive/sys/streams)))

(in-package :alive/file)


(defun do-cmd (cmd path &key stdin-fn stdout-fn stderr-fn)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)
        (funcall cmd path)))


(defun xyz-do-compile (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:xyz-do-compile path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(defun xyz-do-load (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:xyz-do-load path :stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn))


(defun xyz-try-compile (path)
    (do-cmd #+sbcl 'alive/sbcl/file:xyz-try-compile
            path))
