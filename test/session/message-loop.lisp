(defpackage :alive/test/session/message-loop
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:msg-loop :alive/session/message-loop)
                      (:state :alive/session/state)))

(in-package :alive/test/session/message-loop)


(defun check-send (state expected &key read-fn)
    (let* ((send-called nil)
           (deps (deps:create :send-msg (lambda (msg)
                                            (declare (ignore msg))
                                            (setf send-called T)
                                            nil)
                              :read-msg read-fn)))
        (state:set-running state T)
        (msg-loop:run deps state)
        (clue:check-equal :expected expected
                          :actual send-called)))


(defun test-valid-message ()
    (clue:test "Valid message"
        (let* ((state (state:create))
               (deps (deps:create :read-msg (lambda ()
                                                (msg-loop:stop state)
                                                (list (cons :id 5)))
                                  :send-msg (lambda (msg)
                                                (declare (ignore msg))))))
            (msg-loop:run deps state))))


(defun run-all ()
    (clue:suite "Message Loop Tests"
        (test-valid-message)))
