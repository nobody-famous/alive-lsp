(defpackage :alive/test/session/state
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:state :alive/session/state)))

(in-package :alive/test/session/state)


(defun test-add-history ()
    (clue:suite "History"
        (clue:test "Add"
            (let ((state (state:create)))
                (state:add-history state 5)
                (clue:check-equal :expected 5
                                  :actual (state:get-history-item state 0))))

        (clue:test "Get out of range"
            (let ((state (state:create)))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item state 10))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item state -10))))))


(defun test-add-listener ()
    (clue:test "Add"
        (let ((listener (make-instance 'state:listener)))
            (let ((state (state:create)))
                (state:add-listener state listener)
                (clue:check-equal :expected 1
                                  :actual (length (state:listeners state)))))))


(defun test-set-initialized ()
    (clue:suite "Set initialized"
        (clue:test "True"
            (let ((state (state:create)))
                (state:set-initialized state T)
                (clue:check-equal :expected T
                                  :actual (state:initialized state))))))


(defun test-set-file-text ()
    (clue:test "Set file text"
        (let ((state (state:create)))
            (state:set-file-text state "uri" "Some text")
            (clue:check-equal :expected "Some text"
                              :actual (state:get-file-text state "uri")))))


(defun test-set-send-msg-callback ()
    (clue:test "Set send message callback"
        (let ((state (state:create)))
            (state:set-sent-msg-callback state 5 (lambda (msg)
                                                         (declare (ignore msg))
                                                         (make-hash-table)))
            (clue:check-exists (state:get-sent-msg-callback state 5)))))


(defun test-id (label fn)
    (clue:test label
        (let ((state (state:create)))
            (clue:check-equal :expected 1
                              :actual (funcall fn state))
            (clue:check-equal :expected 2
                              :actual (funcall fn state)))))


(defun test-next-send-id ()
    (test-id "Next send id" 'state:next-send-id))


(defun test-next-inspector-id ()
    (test-id "Next inspector id" 'state:next-inspector-id))


(defun test-next-thread-id ()
    (test-id "Next thread id" 'state:next-thread-id))


(defun test-inspector ()
    (clue:test "Inspector"
        (let ((inspector (alive/inspector:create :text "text" :pkg "pkg")))
            (let ((state (state:create)))
                (state:add-inspector state 5 inspector)
                (clue:check-equal :expected inspector :actual (state:get-inspector state 5))

                (state:rem-inspector state 5)
                (clue:check-equal :expected nil :actual (state:get-inspector state 5))))))


(defun test-running ()
    (clue:test "Running"
        (let ((state (state:create)))
            (clue:check-equal :expected nil
                              :actual (state:running state))
            (state:set-running state T)
            (clue:check-equal :expected T
                              :actual (state:running state)))))


(defun test-thread-msg ()
    (clue:test "Thread Message"
        (let ((state (state:create))
              (deps (deps:create)))
            (state:with-thread-msg (state deps 5)
                nil))))


(defun test-create-listener ()
    (clue:test "Create listener"
        (clue:check-equal :expected T
                          :actual (typep (state:create-listener (lambda ())) 'state:listener))))


(defun run-all ()
    (clue:suite "Session State Tests"
        (test-add-history)
        (test-add-listener)
        (test-set-initialized)
        (test-set-file-text)
        (test-set-send-msg-callback)
        (test-next-inspector-id)
        (test-next-send-id)
        (test-next-thread-id)
        (test-inspector)
        (test-running)
        (test-thread-msg)
        (test-create-listener)))
