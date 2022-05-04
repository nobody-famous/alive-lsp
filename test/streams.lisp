(defpackage :alive/test/streams
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:astreams :alive/streams)
                      (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/streams)


(defparameter *test-string* "Test String")


(defun stdout ()
    (run:test "Stdout Test"
              (lambda ()
                  (let* ((out (astreams:make-output-stream))
                         (*standard-output* out)
                         (out-text nil))

                      (astreams:add-listener out (lambda (data)
                                                     (setf out-text data)))

                      (format T "~A" *test-string*)
                      (astreams:flush-stream out)

                      (check:are-equal *test-string* out-text)))))


(defun stdin ()
    (run:test "Stdin Test"
              (lambda ()
                  (let* ((in-stream (astreams:make-input-stream))
                         (*standard-input* in-stream)
                         (out-text nil))

                      (astreams:set-listener in-stream (lambda ()
                                                           *test-string*))

                      (astreams:add-to-input in-stream *test-string*)
                      (setf out-text (read-line))

                      (check:are-equal *test-string* out-text)))))


(defun run-all ()
    (run:suite "Alive Streams Tests"
               (lambda ()
                   (stdout)
                   (stdin))))