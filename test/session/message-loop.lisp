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


(defmacro with-check-send ((expected) &body body)
    (let ((send-called (gensym)))
        `(let ((,send-called nil))
             (state:with-state (state:create :send-msg (lambda (msg)
                                                           (declare (ignore msg))
                                                           (setf ,send-called T)
                                                           nil))
                 (progn ,@body)
                 (clue:check-equal :expected ,expected
                                   :actual ,send-called)))))


(defun loop-test ()
    (state:with-state (state:create)
        (msg-loop:run)))


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
            (state:with-state (state:create)
                (context:with-context (:input-stream (utils:stream-from-string (utils:create-msg "{\"id\":5}")))
                    (msg-loop:run))))

        (clue:test "No handler"
            (state:with-state (state:create)
                (context:with-context (:input-stream (utils:stream-from-string (utils:create-msg "{\"id\":5}")))
                    (msg-loop:run))))))


(defun test-errors ()
    (clue:suite "Errors"
        (clue:test "EOF"
            (with-check-send (nil)
                (context:with-context (:input-stream (make-instance 'eof-stream))
                    (msg-loop:run))))

        (clue:test "Server error no id"
            (with-check-send (nil)
                (context:with-context (:input-stream (make-instance 'server-error-stream :to-throw 'errors:server-error))
                    (msg-loop:run))))

        (clue:test "Server error with id"
            (with-check-send (T)
                (context:with-context (:input-stream (make-instance 'server-error-stream :id 10 :to-throw 'errors:server-error))
                    (msg-loop:run))))

        (clue:test "Unhandled request no id"
            (with-check-send (nil)
                (context:with-context (:input-stream (make-instance 'server-error-stream :to-throw 'errors:unhandled-request))
                    (msg-loop:run))))

        (clue:test "Unhandled request with id"
            (with-check-send (T)
                (context:with-context (:input-stream (make-instance 'server-error-stream :id 10 :to-throw 'errors:unhandled-request))
                    (msg-loop:run))))))


(defun run-all ()
    (clue:suite "Message Loop Tests"
        (test-run)
        (test-valid-message)
        (test-errors)))
