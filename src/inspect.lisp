(defpackage :alive/inspect
    (:use :cl)
    (:export :from-string)
    (:local-nicknames (:astreams :alive/streams)
                      (:pkgs :alive/packages)))

(in-package :alive/inspect)


(defun from-string (str &key pkg-name stdin-fn stdout-fn stderr-fn)
    (let* ((orig-stdin *standard-input*)
           (orig-stdout *standard-output*)
           (orig-stderr *error-output*)
           (io-stream (astreams:make-io-stream))
           (*query-io* io-stream)
           (*standard-input* io-stream)
           (*standard-output* io-stream)
           (*error-output* io-stream)
           (input (make-string-input-stream str))
           (pkg (pkgs:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (when stdin-fn
              (astreams:set-in-listener io-stream
                                        (lambda ()
                                            (let ((*standard-input* orig-stdin)
                                                  (*standard-output* orig-stdout)
                                                  (*error-output* orig-stderr))
                                                (funcall stdin-fn)))))

        (when stdout-fn
              (astreams:set-out-listener io-stream
                                         (lambda (data)
                                             (let ((*standard-output* orig-stdout)
                                                   (*error-output* orig-stderr))
                                                 (funcall stdout-fn data)))))

        (when stderr-fn
              (astreams:set-out-listener io-stream
                                         (lambda (data)
                                             (let ((*standard-output* orig-stdout)
                                                   (*error-output* orig-stderr))
                                                 (funcall stderr-fn data)))))

        (when (and pkg-name (not pkg))
              (error (make-condition 'pkgs:package-not-found :name pkg-name)))

        (unwind-protect
                (eval (read input))
            (astreams:flush-out-stream io-stream))))