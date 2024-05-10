(defpackage :alive/test/session/state
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:state :alive/session/state)))

(in-package :alive/test/session/state)


(defun test-add-history ()
    (clue:suite "History"
        (clue:test "Add"
            (clue:expect-fail (lambda () (state:get-history-item 0)))
            (clue:expect-fail (lambda () (state:add-history 5)))
            (state:with-state (state:create)
                (state:add-history 5)
                (clue:check-equal :expected 5
                                  :actual (state:get-history-item 0))))

        (clue:test "Get out of range"
            (state:with-state (state:create)
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item 10))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item -10))))))


(defun test-add-listener ()
    (clue:test "Add"
        (let ((listener (make-instance 'state:listener)))
            (clue:expect-fail (lambda () (state:listeners)))
            (clue:expect-fail (lambda () (state:add-listener listener)))
            (state:with-state (state:create)
                (state:add-listener listener)
                (clue:check-equal :expected 1
                                  :actual (length (state:listeners)))))))


(defun test-set-initialized ()
    (clue:suite "Set initialized"
        (clue:expect-fail (lambda () (state:initialized)))
        (clue:expect-fail (lambda () (state:set-initialized nil)))
        (clue:test "True"
            (state:with-state (state:create)
                (state:set-initialized T)
                (clue:check-equal :expected T
                                  :actual (state:initialized))))))


(defun test-set-file-text ()
    (clue:test "Set file text"
        (clue:expect-fail (lambda () (state:get-file-text "foo")))
        (clue:expect-fail (lambda () (state:set-file-text "foo" "bar")))
        (state:with-state (state:create)
            (state:set-file-text "uri" "Some text")
            (clue:check-equal :expected "Some text"
                              :actual (state:get-file-text "uri")))))


(defun test-next-send-id ()
    (clue:test "Next send id"
        (clue:expect-fail (lambda () (state:next-send-id)))
        (state:with-state (state:create)
            (clue:check-equal :expected 1
                              :actual (state:next-send-id))
            (clue:check-equal :expected 2
                              :actual (state:next-send-id)))))


(defun test-next-inspector-id ()
    (clue:test "Next inspector id"
        (clue:expect-fail (lambda () (state:next-inspector-id)))
        (state:with-state (state:create)
            (clue:check-equal :expected 1
                              :actual (state:next-inspector-id))
            (clue:check-equal :expected 2
                              :actual (state:next-inspector-id)))))


(defun test-inspector ()
    (clue:test "Inspector"
        (let ((inspector (alive/inspector:create :text "text" :pkg "pkg")))
            (clue:expect-fail (lambda () (state:get-inspector 5)))
            (clue:expect-fail (lambda () (state:rem-inspector 5)))
            (clue:expect-fail (lambda () (state:add-inspector 5 inspector)))
            (state:with-state (state:create)
                (state:add-inspector 5 inspector)
                (clue:check-equal :expected inspector :actual (state:get-inspector 5))

                (state:rem-inspector 5)
                (clue:check-equal :expected nil :actual (state:get-inspector 5))))))


(defun test-running ()
    (clue:test "Running"
        (clue:expect-fail (lambda () (state:running)))
        (clue:expect-fail (lambda () (state:set-running nil)))
        (state:with-state (state:create)
            (clue:check-equal :expected nil
                              :actual (state:running))
            (state:set-running T)
            (clue:check-equal :expected T
                              :actual (state:running)))))


(defun test-lock ()
    (clue:test "Lock"
        (clue:expect-fail (lambda () (state:lock)))
        (state:with-state (state:create)
            (clue:check-exists (state:lock)))))


(defun test-thread-msg ()
    (clue:test "Thread Message"
        (clue:expect-fail (lambda () (state:with-thread-msg (5))))
        (state:with-state (state:create)
            (state:with-thread-msg (5)
                nil))))


(defun run-all ()
    (clue:suite "Session State Tests"
        (test-add-history)
        (test-add-listener)
        (test-set-initialized)
        (test-set-file-text)
        (test-next-send-id)
        (test-next-inspector-id)
        (test-inspector)
        (test-running)
        (test-lock)
        (test-thread-msg)))
