(defpackage :alive/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
    (:local-nicknames (:astreams :alive/streams)))

(in-package :alive/file)


(defun do-cmd (cmd path &key stdin-fn stdout-fn stderr-fn)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn)
        (funcall cmd path)))


(defun do-compile (path &key stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:do-compile
            path
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))


(defun do-load (path &key stdin-fn stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:do-load
            path
            :stdin-fn stdin-fn
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))


(defun try-compile (path &key stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:try-compile
            path
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))
