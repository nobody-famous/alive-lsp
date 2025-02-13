(defpackage :alive/test/session/state
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:state :alive/session/state)))

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


(defun test-get-files ()
    nil)


(defun test-set-send-msg-callback ()
    (clue:test "Set file text"
        (clue:expect-fail (lambda () (state:get-sent-msg-callback 5)))
        (clue:expect-fail (lambda () (state:set-sent-msg-callback 5 (lambda (msg)
                                                                        (declare (ignore msg))
                                                                        (make-hash-table)))))
        (state:with-state (state:create)
            (state:set-sent-msg-callback 5 (lambda (msg)
                                               (declare (ignore msg))
                                               (make-hash-table)))
            (clue:check-exists (state:get-sent-msg-callback 5)))))


(defmacro test-id (label fn)
    `(clue:test ,label
         (clue:expect-fail (lambda () (,fn)))
         (state:with-state (state:create)
             (clue:check-equal :expected 1
                               :actual (,fn))
             (clue:check-equal :expected 2
                               :actual (,fn)))))


(defun test-next-send-id ()
    (test-id "Next send id" state:next-send-id))


(defun test-next-inspector-id ()
    (test-id "Next inspector id" state:next-inspector-id))


(defun test-next-thread-id ()
    (test-id "Next thread id" state:next-thread-id))


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


(defun test-thread-msg ()
    (clue:test "Thread Message"
        #+n (clue:expect-fail (lambda () (state:with-thread-msg (5))))
        (state:with-state (state:create)
            (deps:with-deps (deps:create)
                (state:with-thread-msg (5)
                    nil)))))


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
