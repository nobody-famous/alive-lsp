(defpackage :alive/test/deps
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)))

(in-package :alive/test/deps)


(defun test-msg-handler ()
    (clue:test "Message handler"
        (let ((deps (deps:new-create :msg-handler (lambda (deps msg) (declare (ignore deps msg))))))
            (clue:check-equal :expected T
                              :actual (functionp (deps:new-msg-handler deps))))))


(defun test-read-msg ()
    (clue:test "Read message"
        (let ((deps (deps:new-create :read-msg (lambda () (list 1 2)))))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:new-read-msg deps)))))


(defun test-send-msg ()
    (clue:test "Send message"
        (let* ((sent nil)
               (deps (deps:new-create :send-msg (lambda (msg)
                                                    (declare (ignore msg))
                                                    (setf sent T)
                                                    nil))))
            (deps:new-send-msg deps 5)
            (clue:check-equal :expected t :actual sent))))


(defun test-send-request ()
    (clue:test "Send request"
        (let ((deps (deps:new-create :send-request (lambda (msg)
                                                       (declare (ignore msg))
                                                       (list (cons :result "foo"))))))
            (clue:check-equal :expected "foo"
                              :actual (cdr (assoc :result (deps:new-send-request deps (make-hash-table))))))))


(defun test-list-all-threads ()
    (clue:test "Lilst all threads"
        (let ((deps (deps:new-create :list-all-threads (lambda () (list 1 2)))))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:new-list-all-threads deps)))))


(defun test-kill-thread ()
    (clue:test "Kill thread"
        (let ((deps (deps:new-create)))
            (deps:new-kill-thread deps nil))))


(defun test-get-thread-id ()
    (clue:test "Get thread id"
        (let ((deps (deps:new-create :get-thread-id (lambda (thread)
                                                        (declare (ignore thread))
                                                        5))))
            (clue:check-equal :expected 5
                              :actual (deps:new-get-thread-id deps (bt:current-thread))))))


(defun test-list-all-asdf ()
    (clue:test "List all ASDF systems"
        (let ((deps (deps:new-create :list-all-asdf (lambda () (list 1 2)))))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:new-list-all-asdf deps)))))


(defun test-load-asdf ()
    (clue:test "Load ASDF system"
        (let ((deps (deps:new-create :list-all-asdf (lambda () (list 1 2)))))
            (clue:check-equal :expected T
                              :actual (deps:new-load-asdf-system deps)))))


(defun test-do-eval ()
    (clue:test "Eval"
        (let ((deps (deps:new-create :eval-fn (lambda (data) data))))
            (clue:check-equal :expected (list "foo")
                              :actual (deps:new-do-eval deps "foo")))))


(defun test-macro-expand ()
    (clue:test "Macro expand"
        (let ((deps (deps:new-create)))
            (deps:new-macro-expand deps "foo" "bar"))))


(defun test-macro-expand-1 ()
    (clue:test "Macro expand 1"
        (let ((deps (deps:new-create)))
            (deps:new-macro-expand-1 deps "foo" "bar"))))


(defun test-try-compile ()
    (clue:test "Try compile"
        (let ((deps (deps:new-create)))
            (deps:new-try-compile deps "foo"))))


(defun test-do-compile ()
    (clue:test "Do compile"
        (let ((deps (deps:new-create)))
            (deps:new-do-compile deps "foo"))))


(defun test-do-load ()
    (clue:test "Do load"
        (let ((deps (deps:new-create)))
            (deps:new-do-load deps "foo"))))


(defun run-all ()
    (clue:suite "Dependency Tests"
        (test-msg-handler)
        (test-read-msg)
        (test-send-msg)
        (test-send-request)
        (test-list-all-threads)
        (test-kill-thread)
        (test-get-thread-id)
        (test-list-all-asdf)
        (test-load-asdf)
        (test-do-eval)
        (test-macro-expand)
        (test-macro-expand-1)
        (test-try-compile)
        (test-do-compile)
        (test-do-load)))
