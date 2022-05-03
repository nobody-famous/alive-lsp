(defpackage :alive/test/streams
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:astream :alive/streams)
                      (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/streams)


(defparameter *test-string* "Test String")


(defun stdout ()
    (run:test "Stdout Test"
              (lambda ()
                  (let* ((out (astream:make-stream))
                         (*standard-output* out)
                         (out-text nil))

                      (astream:add-listener out (lambda (data)
                                                    (setf out-text data)))

                      (format T "~A" *test-string*)
                      (astream:flush-stream out)

                      (check:are-equal *test-string* out-text)))))


(defun run-all ()
    (run:suite "Alive Streams Tests"
               (lambda ()
                   (stdout))))