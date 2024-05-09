(defpackage :alive/test/session/message-loop
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:errors :alive/lsp/errors)
                      (:msg-loop :alive/session/message-loop)
                      (:state :alive/session/state)))

(in-package :alive/test/session/message-loop)


(defmacro with-check-send ((expected &key read-fn) &body body)
    (let ((send-called (gensym)))
        `(let ((,send-called nil))
             (state:with-state (state:create :send-msg (lambda (msg)
                                                           (declare (ignore msg))
                                                           (setf ,send-called T)
                                                           nil)
                                             :read-msg ,read-fn)
                 (progn ,@body)
                 (clue:check-equal :expected ,expected
                                   :actual ,send-called)))))


(defun test-valid-message ()
    (clue:suite "Valid message"
        (clue:test "With handler"
            (state:with-state (state:create :read-msg (lambda ()
                                                          (msg-loop:stop)
                                                          (list (cons :id 5))))
                (msg-loop:run)))

        (clue:test "No handler"
            (state:with-state (state:create :read-msg (lambda ()
                                                          (msg-loop:stop)
                                                          (list (cons :id 5))))
                (msg-loop:run)))))


(defun test-errors ()
    (clue:suite "Errors"
        (clue:test "EOF"
            (with-check-send (nil :read-fn (lambda () (error (make-instance 'end-of-file))))
                (msg-loop:run)))

        (clue:test "Server error no id"
            (with-check-send (nil :read-fn (lambda ()
                                               (msg-loop:stop)
                                               (error (make-instance 'errors:server-error))))
                (msg-loop:run)))

        (clue:test "Server error with id"
            (with-check-send (T :read-fn (lambda ()
                                             (msg-loop:stop)
                                             (error (make-instance 'errors:server-error :id 10))))
                (msg-loop:run)))

        (clue:test "Unhandled request no id"
            (with-check-send (nil :read-fn (lambda ()
                                               (msg-loop:stop)
                                               (error (make-instance 'errors:unhandled-request))))
                (msg-loop:run)))

        (clue:test "Unhandled request with id"
            (with-check-send (T :read-fn (lambda ()
                                             (msg-loop:stop)
                                             (error (make-instance 'errors:unhandled-request :id 10))))
                (msg-loop:run)))

        (clue:test "Generic error"
            (with-check-send (nil :read-fn (lambda ()
                                               (msg-loop:stop)
                                               (error "Failed, as requested")))
                (msg-loop:run)))))


(defun run-all ()
    (clue:suite "Message Loop Tests"
        (test-valid-message)
        (test-errors)))
