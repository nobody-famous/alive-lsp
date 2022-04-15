(defpackage :alive/eval
    (:use :cl)
    (:export :from-string)
    (:local-nicknames (:astreams :alive/streams)
                      (:pkgs :alive/packages)))

(in-package :alive/eval)


(defun from-string (str &key pkg-name stdout-fn stderr-fn)
    (let* ((orig-stdout *standard-output*)
           (orig-stderr *error-output*)
           (out-stream (astreams:make-stream))
           (err-stream (astreams:make-stream))
           (*standard-output* out-stream)
           (*error-output* err-stream)
           (input (make-string-input-stream str))
           (pkg (pkgs:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

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

        (when (and pkg-name (not pkg))
              (error (make-condition 'pkgs:package-not-found :name pkg-name)))

        (unwind-protect
                (eval (read input))
            (astreams:flush-stream out-stream)
            (astreams:flush-stream err-stream))))
