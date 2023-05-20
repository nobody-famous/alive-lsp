(defpackage :alive/test/streams
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:astreams :alive/streams)))

(in-package :alive/test/streams)


(defparameter *test-string* "Test String")


(defun test-stdout ()
    (clue:test "Stdout Test"
        (let* ((out (astreams:make-io-stream))
               (*standard-output* out)
               (out-text nil))

            (astreams:set-out-listener out (lambda (data)
                                               (setf out-text data)))

            (format T "~A" *test-string*)
            (astreams:flush-out-stream out)

            (clue:check-equal :expected *test-string*
                              :actual out-text))))


(defun test-stdin ()
    (clue:test "Stdin Test"
        (let* ((in-stream (astreams:make-io-stream))
               (*standard-input* in-stream)
               (listener-called nil)
               (out-text nil))

            (astreams:set-in-listener in-stream (lambda ()
                                                    (let ((return-eof listener-called))
                                                        (setf listener-called T)
                                                        (if return-eof :eof *test-string*))))

            (setf out-text (read-line))

            (clue:check-equal :expected *test-string*
                              :actual out-text))))


(defun test-io-stream ()
    (clue:test "IO Stream"
        (let ((io (alive/streams:make-io-stream)))
            (unwind-protect
                    (progn (clue:check-equal :expected 'character
                                             :actual (stream-element-type io))

                           (clue:check-equal :expected #\newline
                                             :actual (read-char io)))
                (close io)))))


(defun test-unread ()
    (clue:test "Unread char"
        (let ((io (alive/streams:make-io-stream)))
            (unwind-protect
                    (progn (astreams:set-in-listener io (lambda () "a"))
                           (clue:check-equal :expected #\a
                                             :actual (read-char io))

                           (sb-gray:stream-unread-char io #\a)
                           (clue:check-equal :expected #\a
                                             :actual (read-char io))

                           (clue:check-equal :expected ""
                                             :actual (read-line io))

                           (astreams:set-in-listener io (lambda () "def"))
                           (clue:check-equal :expected #\d
                                             :actual (read-char io))

                           (sb-gray:stream-unread-char io #\a)
                           (clue:check-equal :expected "aef"
                                             :actual (read-line io)))
                (close io)))))


(defun run-all ()
    (clue:suite "Alive Streams Tests"
        (test-io-stream)
        (test-unread)
        (test-stdout)
        (test-stdin)))