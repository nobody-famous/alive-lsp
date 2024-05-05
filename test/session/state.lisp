(defpackage :alive/test/session/state
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:state :alive/session/state)))

(in-package :alive/test/session/state)


(defun test-add-history ()
    (clue:suite "History"
        (clue:test "Add"
            (state:with-state (make-instance 'state:state)
                (state:add-history 5)
                (clue:check-equal :expected 5
                                  :actual (state:get-history-item 0))))

        (clue:test "Get out of range"
            (state:with-state (make-instance 'state:state)
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item 10))
                (clue:check-equal :expected nil
                                  :actual (state:get-history-item -10))))))


(defun test-add-listener ()
    (clue:test "Add"
        (state:with-state (make-instance 'state:state)
            (let ((listener (make-instance 'state:listener)))
                (state:add-listener listener)
                (clue:check-equal :expected 1
                                  :actual (length (state:listeners)))))))


(defun test-set-initialized ()
    (clue:suite "Set initialized"
        (clue:test "True"
            (state:with-state (make-instance 'state:state)
                (state:set-initialized T)
                (clue:check-equal :expected T
                                  :actual (state:initialized))))))


(defun test-set-file-text ()
    (clue:test "Set file text"
        (state:with-state (make-instance 'state:state)
            (state:set-file-text "uri" "Some text")
            (clue:check-equal :expected "Some text"
                              :actual (state:get-file-text "uri")))))


(defun test-next-send-id ()
    (clue:test "Next send id"
        (state:with-state (make-instance 'state:state)
            (clue:check-equal :expected 1
                              :actual (state:next-send-id))
            (clue:check-equal :expected 2
                              :actual (state:next-send-id)))))


(defun test-next-inspector-id ()
    (clue:test "Next inspector id"
        (state:with-state (make-instance 'state:state)
            (clue:check-equal :expected 1
                              :actual (state:next-inspector-id))
            (clue:check-equal :expected 2
                              :actual (state:next-inspector-id)))))


(defun test-inspector ()
    (clue:test "Inspector"
        (state:with-state (make-instance 'state:state)
            (let ((inspector (alive/inspector:create :text "text" :pkg "pkg")))
                (state:add-inspector 5 inspector)
                (clue:check-equal :expected inspector :actual (state:get-inspector 5))

                (state:rem-inspector 5)
                (clue:check-equal :expected nil :actual (state:get-inspector 5))))))


(defun run-all ()
    (clue:suite "Session State Tests"
        (test-add-history)
        (test-add-listener)
        (test-set-initialized)
        (test-set-file-text)
        (test-next-send-id)
        (test-next-inspector-id)
        (test-inspector)))
