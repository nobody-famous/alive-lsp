(defpackage :alive/file
    (:use :cl)
    (:export :do-compile
             :do-load
             :try-compile)
    (:local-nicknames (:astreams :alive/streams)))

(in-package :alive/file)


(defun do-cmd (cmd path &key stdout-fn stderr-fn)
    (let* ((orig-stdout *standard-output*)
           (orig-stderr *error-output*)
           (out-stream (astreams:make-output-stream))
           (err-stream (astreams:make-output-stream))
           (*standard-output* out-stream)
           (*error-output* err-stream))

        (astreams:add-listener out-stream
                               (lambda (data)
                                   (when stdout-fn
                                         (let ((*standard-output* orig-stdout)
                                               (*error-output* orig-stderr))
                                             (funcall stdout-fn data)))))

        (astreams:add-listener err-stream
                               (lambda (data)
                                   (when stderr-fn
                                         (let ((*standard-output* orig-stdout)
                                               (*error-output* orig-stderr))
                                             (funcall stderr-fn data)))))

        (unwind-protect
                (funcall cmd path)
            (astreams:flush-stream out-stream)
            (astreams:flush-stream err-stream))))


(defun do-compile (path &key stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:do-compile
            path
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))


(defun do-load (path &key stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:do-load
            path
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))


(defun try-compile (path &key stdout-fn stderr-fn)
    (do-cmd #+sbcl 'alive/sbcl/file:try-compile
            path
            :stdout-fn stdout-fn
            :stderr-fn stderr-fn))
