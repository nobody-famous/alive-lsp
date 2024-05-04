(defpackage :alive/test/session/state
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:state :alive/session/state)))

(in-package :alive/test/session/state)


(defun test-add-history ()
    (clue:suite "History"
        (clue:test "Add"
            (let ((state (make-instance 'state:state)))
                (state:add-history state 5)
                (clue:check-equal :expected 5
                                  :actual (state:get-history-item state 0))))

        (clue:test "Get out of range"
            (let ((state (make-instance 'state:state)))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item state 10))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item state -10))))

        (clue:test "Invalid index"
            (let ((state (make-instance 'state:state)))
                (clue:expect-fail (lambda () (state:get-history-item state "foo")))))))


(defun test-add-listener ()
    (clue:test "Add"
        (let ((state (make-instance 'state:state))
              (listener (make-instance 'state:listener)))
            (state:add-listener state listener)
            (clue:check-equal :expected 1
                              :actual (length (state:listeners state))))))


(defun test-set-initialized ()
    (clue:suite "Set initialized"
        (clue:test "True"
            (let ((state (make-instance 'state:state)))
                (state:set-initialized state T)
                (clue:check-equal :expected T
                                  :actual (state:initialized state))))
        (clue:test "Invalid"
            (let ((state (make-instance 'state:state)))
                (clue:expect-fail (lambda () (state:set-initialized state 5)))))))


(defun test-set-file-text ()
    (clue:test "Set file text"
        (let ((state (make-instance 'state:state)))
            (state:set-file-text state "uri" "Some text")
            (clue:check-equal :expected "Some text"
                              :actual (state:get-file-text state "uri")))))


(defun test-next-send-id ()
    (clue:test "Nexst send id"
        (let ((state (make-instance 'state:state)))
            (clue:check-equal :expected 1
                              :actual (state:next-send-id state))
            (clue:check-equal :expected 2
                              :actual (state:next-send-id state)))))


(defun test-next-inspector-id ()
    (clue:test "Nexst inspector id"
        (let ((state (make-instance 'state:state)))
            (clue:check-equal :expected 1
                              :actual (state:next-inspector-id state))
            (clue:check-equal :expected 2
                              :actual (state:next-inspector-id state)))))


(defun test-inspector ()
    (clue:test "Inspector"
        (let ((state (make-instance 'state:state))
              (inspector (alive/inspector:create :text "text" :pkg "pkg")))
            (state:add-inspector state :id 5 :inspector inspector)
            (clue:check-equal :expected inspector :actual (state:get-inspector state :id 5))

            (state:rem-inspector state :id 5)
            (clue:check-equal :expected nil :actual (state:get-inspector state :id 5)))))


(defun run-all ()
    (clue:suite "Session State Tests"
        (test-add-history)
        (test-add-listener)
        (test-set-initialized)
        (test-set-file-text)
        (test-next-send-id)
        (test-next-inspector-id)
        (test-inspector)))
