(defpackage :alive/sys/streams
    (:use :cl)
    (:export :set-in-listener
             :set-out-listener
             :flush-out-stream
             :make-io-stream
             :with-redirect-streams))

(in-package :alive/sys/streams)


(defun make-io-stream ()
    #+sbcl (make-instance 'alive/sbcl/streams:io-stream))


(defun flush-out-stream (obj)
    #+sbcl (alive/sbcl/streams:flush-out-buffer obj))


(defun set-in-listener (obj listener)
    #+sbcl (alive/sbcl/streams:set-in-listener obj listener))


(defun set-out-listener (obj listener)
    #+sbcl (alive/sbcl/streams:set-out-listener obj listener))


(defmacro with-redirect-streams ((&key stdin-fn stdout-fn stderr-fn query-fn trace-fn) &body body)
    (let ((orig-stdin (gensym))
          (orig-stdout (gensym))
          (orig-trace (gensym))
          (orig-stderr (gensym))
          (io-stream (gensym))
          (query-stream (gensym))
          (trace-stream (gensym))
          (err-stream (gensym)))
        `(let* ((,orig-stdin *standard-input*)
                (,orig-stdout *standard-output*)
                (,orig-trace *trace-output*)
                (,orig-stderr *error-output*)
                (,io-stream (make-io-stream))
                (,query-stream (make-io-stream))
                (,trace-stream (make-io-stream))
                (,err-stream (make-io-stream))
                (*query-io* ,query-stream)
                (*standard-input* ,io-stream)
                (*standard-output* ,io-stream)
                (*trace-output* ,trace-stream)
                (*error-output* ,err-stream))

             (when ,stdin-fn
                   (set-in-listener ,io-stream
                                    (lambda ()
                                        (let ((*standard-input* ,orig-stdin)
                                              (*standard-output* ,orig-stdout)
                                              (*error-output* ,orig-stderr))
                                            (funcall ,stdin-fn))))

                   (set-in-listener ,query-stream
                                    (lambda ()
                                        (let ((*standard-input* ,orig-stdin)
                                              (*standard-output* ,orig-stdout)
                                              (*error-output* ,orig-stderr))
                                            (funcall ,stdin-fn)))))

             (when ,query-fn
                   (set-out-listener ,query-stream
                                     (lambda (data)
                                         (let ((*standard-output* ,orig-stdout)
                                               (*error-output* ,orig-stderr))
                                             (funcall ,query-fn data)))))

             (when ,stdout-fn
                   (set-out-listener ,io-stream
                                     (lambda (data)
                                         (let ((*standard-output* ,orig-stdout)
                                               (*error-output* ,orig-stderr))
                                             (funcall ,stdout-fn data)))))

             (when ,trace-fn
                   (set-out-listener ,trace-stream
                                     (lambda (data)
                                         (let ((*standard-output* ,orig-stdout)
                                               (*trace-output* ,orig-trace)
                                               (*error-output* ,orig-stderr))
                                             (funcall ,trace-fn data)))))

             (when ,stderr-fn
                   (set-out-listener ,err-stream
                                     (lambda (data)
                                         (let ((*standard-output* ,orig-stdout)
                                               (*error-output* ,orig-stderr))
                                             (funcall ,stderr-fn data)))))

             (unwind-protect
                     (progn ,@body)
                 (flush-out-stream ,io-stream)
                 (flush-out-stream ,err-stream)))))
