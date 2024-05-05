(defpackage :alive/test/session/message-loop
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:context :alive/context)
                      (:errors :alive/lsp/errors)
                      (:logger :alive/logger)
                      (:msg-loop :alive/session/message-loop)
                      (:state :alive/session/state)
                      (:utils :alive/test/utils)))

(in-package :alive/test/session/message-loop)


(defclass output-stream (stream)
        ((seq :accessor seq
              :initform nil
              :initarg :seq)))

(defmethod sb-gray:stream-write-sequence ((s output-stream) seq &optional start end)
    (declare (ignore start end))
    (setf (seq s) seq))

(defmethod sb-gray:stream-force-output ((s output-stream))
    nil)


(defclass eof-stream (stream)
        ())

(defmethod sb-gray:stream-read-byte ((s eof-stream))
    (error (make-instance 'end-of-file)))


(defclass server-error-stream (stream)
        ((id :accessor id
             :initform nil
             :initarg :id)
         (to-throw :accessor to-throw
                   :initform nil
                   :initarg :to-throw)))

(defmethod sb-gray:stream-read-byte ((s server-error-stream))
    (msg-loop:stop)
    (error (make-instance (to-throw s) :id (id s))))


(defun make-state ()
    (state:create #'alive/session/message:handle))


(defun loop-test ()
    (state:with-state (make-state)
        (let ((destroy-called nil)
              (in-stream (utils:stream-from-string (utils:create-msg "foo"))))
            (context:with-context (:input-stream in-stream
                                                 :destroy-fn (lambda ()
                                                                 (setf destroy-called T)))
                (msg-loop:run)
                (clue:check-equal :expected T
                                  :actual destroy-called)))))


(defun test-run ()
    (clue:suite "Run"
        (clue:test "With logger"
            (let ((result nil))
                (with-output-to-string (str)
                    (logger:with-logging (logger:create str logger:*info*)
                        (setf result (loop-test))))
                result))

        (clue:test "Without logger"
            (loop-test))))


(defun test-valid-message ()
    (clue:suite "Valid message"
        (clue:test "With handler"
            (let ((in-stream (utils:stream-from-string (utils:create-msg "{\"id\":5}"))))
                (state:with-state (make-state)
                    (context:with-context (:input-stream in-stream
                                                         :destroy-fn (lambda ()))
                        (msg-loop:run)))))

        (clue:test "No handler"
            (let ((in-stream (utils:stream-from-string (utils:create-msg "{\"id\":5}"))))
                (state:with-state (state:create (lambda (msg)
                                                    (declare (ignore msg))
                                                    (error "Failed, as requested")))
                    (context:with-context (:input-stream in-stream
                                                         :destroy-fn (lambda ()))
                        (msg-loop:run)))))))


(defun test-errors ()
    (clue:suite "Errors"
        (clue:test "EOF"
            (let ((out-stream (make-instance 'output-stream)))
                (state:with-state (make-state)
                    (context:with-context (:input-stream (make-instance 'eof-stream)
                                                         :output-stream out-stream
                                                         :destroy-fn (lambda ()))
                        (msg-loop:run)
                        (clue:check-equal :expected nil
                                          :actual (seq out-stream))))))

        (clue:test "Server error no id"
            (let ((out-stream (make-instance 'output-stream)))
                (state:with-state (make-state)
                    (context:with-context (:input-stream (make-instance 'server-error-stream :to-throw 'errors:server-error)
                                                         :output-stream out-stream
                                                         :destroy-fn (lambda ()))
                        (msg-loop:run)
                        (clue:check-equal :expected nil
                                          :actual (seq out-stream))))))

        (clue:test "Server error with id"
            (let ((out-stream (make-instance 'output-stream)))
                (state:with-state (make-state)
                    (context:with-context (:input-stream (make-instance 'server-error-stream :id 10 :to-throw 'errors:server-error)
                                                         :output-stream out-stream
                                                         :destroy-fn (lambda ()))
                        (msg-loop:run)
                        (clue:check-exists (seq out-stream))))))

        (clue:test "Unhandled request no id"
            (let ((out-stream (make-instance 'output-stream)))
                (state:with-state (make-state)
                    (context:with-context (:input-stream (make-instance 'server-error-stream :to-throw 'errors:unhandled-request)
                                                         :output-stream out-stream
                                                         :destroy-fn (lambda ()))
                        (msg-loop:run)
                        (clue:check-equal :expected nil
                                          :actual (seq out-stream))))))

        (clue:test "Unhandled request with id"
            (let ((out-stream (make-instance 'output-stream)))
                (state:with-state (make-state)
                    (context:with-context (:input-stream (make-instance 'server-error-stream :id 10 :to-throw 'errors:unhandled-request)
                                                         :output-stream out-stream
                                                         :destroy-fn (lambda ()))
                        (msg-loop:run)
                        (clue:check-exists (seq out-stream))))))))


(defun run-all ()
    (clue:suite "Message Loop Tests"
        (test-run)
        (test-valid-message)
        (test-errors)))
