(defpackage :alive/asdf
    (:use :cl)
    (:export :list-systems
             :load-system)
    (:local-nicknames (:astreams :alive/streams)))

(in-package :alive/asdf)


(defun list-systems ()
    (mapcar #'string-downcase (asdf:registered-systems)))


(defun load-system (&key name stdout-fn stderr-fn)
    (let* ((orig-stdout *standard-output*)
           (orig-stderr *error-output*)
           (out-stream (astreams:make-stream))
           (err-stream (astreams:make-stream))
           (*standard-output* out-stream)
           (*error-output* err-stream))

        (when stdout-fn
              (astreams:add-listener out-stream
                                     (lambda (data)
                                         (let ((*standard-output* orig-stdout)
                                               (*error-output* orig-stderr))
                                             (funcall stdout-fn data)))))

        (when stderr-fn
              (astreams:add-listener err-stream
                                     (lambda (data)
                                         (let ((*standard-output* orig-stdout)
                                               (*error-output* orig-stderr))
                                             (funcall stderr-fn data)))))

        (asdf:load-system name)))
