(defpackage :alive/test/compat/sbcl/compile
    (:use :cl)
    (:export :run-all)

    (:local-nicknames (:astreams :alive/streams)
                      (:file :alive/file)))

(in-package :alive/test/compat/sbcl/compile)


(defun compile-foo (stdout)
    (file:do-compile (lambda (msg)
                         (format stdout "~&CALLBACK: ~A~%" msg))
                     "test/files/compile/foo.lisp"))


(defun read-stuff (stdout out-stream)
    (lambda ()
        (handler-case
                (loop :until (astreams:eof-p out-stream)
                      :do (let ((line (read-line out-stream)))
                              (unless (eq line :eof)
                                      (format stdout "OUT ~A~%" line))))
            (t (c) (format stdout "~&OUT CAUGHT: ~A~%" c)))))


(defun read-err-stuff (stdout out-stream)
    (lambda ()
        (handler-case
                (loop :until (astreams:eof-p out-stream)
                      :do (let ((line (read-line out-stream)))
                              (unless (eq line :eof)
                                      (format stdout "ERR ~A~%" (string-upcase line)))))
            (t (c) (format stdout "~&ERR CAUGHT: ~A~%" c)))))


(defun report-output (label orig-stdout)
    (lambda (data)
        (format orig-stdout "~A ~A~&" label data)))


(defun run-all ()
    (format T "SBCL Compile Tests~%")

    (let* ((orig-output *standard-output*)
           (orig-err *error-output*)
           (out-stream (astreams:make-stream :stdout orig-output))
           (err-stream (astreams:make-stream :stdout orig-err));    (out-stream (make-instance 'astreams:rt-stream :stdout orig-output));    (err-stream (make-instance 'astreams:rt-stream :stdout orig-err))(*standard-output* out-stream)(*error-output* err-stream))
        ; (compile-file "test/compat/sbcl/files/foo.lisp")
        ; (astreams:add-listener out-stream (report-output "OUT" orig-output))
        ; (astreams:add-listener err-stream (report-output "ERR" orig-err))

        (compile-foo orig-output)

        (close out-stream)
        (close err-stream))))
