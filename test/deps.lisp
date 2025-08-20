(defpackage :alive/test/deps
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)))

(in-package :alive/test/deps)


(defun test-msg-handler ()
    (clue:test "Message handler"
        (let ((deps (deps:create :msg-handler (lambda (deps msg) (declare (ignore deps msg))))))
            (clue:check-equal :expected T
                              :actual (functionp (deps:msg-handler deps))))))


(defun test-read-msg ()
    (clue:test "Read message"
        (let ((deps (deps:create :read-msg (lambda () (list 1 2)))))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:read-msg deps)))))


(defun test-send-msg ()
    (clue:test "Send message"
        (let* ((sent nil)
               (deps (deps:create :send-msg (lambda (msg)
                                                (declare (ignore msg))
                                                (setf sent T)
                                                nil))))
            (deps:send-msg deps 5)
            (clue:check-equal :expected t :actual sent))))


(defun test-send-request ()
    (clue:test "Send request"
        (let ((deps (deps:create :send-request (lambda (msg)
                                                   (declare (ignore msg))
                                                   (list (cons :result "foo"))))))
            (clue:check-equal :expected "foo"
                              :actual (cdr (assoc :result (deps:send-request deps (make-hash-table))))))))


(defun test-list-all-threads ()
    (clue:test "Lilst all threads"
        (let ((deps (deps:create :list-all-threads (lambda () (list 1 2)))))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:list-all-threads deps)))))


(defun test-kill-thread ()
    (clue:test "Kill thread"
        (let ((deps (deps:create)))
            (deps:kill-thread deps nil))))


(defun test-get-thread-id ()
    (clue:test "Get thread id"
        (let ((deps (deps:create :get-thread-id (lambda (thread)
                                                    (declare (ignore thread))
                                                    5))))
            (clue:check-equal :expected 5
                              :actual (deps:get-thread-id deps (bt:current-thread))))))


(defun test-list-all-asdf ()
    (clue:test "List all ASDF systems"
        (let ((deps (deps:create :list-all-asdf (lambda () (list 1 2)))))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:list-all-asdf deps)))))


(defun test-list-all-traced ()
    (clue:test "List all traced functions"
        (let ((deps (deps:create :list-all-traced (lambda () (list 1 2)))))
            (clue:check-equal :expected (list 1 2)
                              :actual (deps:list-all-traced deps)))))


(defun test-trace-fn ()
    (clue:suite "Trace function"
        (clue:test "Success"
            (let ((deps (deps:create :trace-fn (lambda (fn-name) (declare (ignore fn-name)) T))))
                (clue:check-equal :expected T
                                  :actual (deps:trace-fn deps "bar"))))
        (clue:test "Fail"
            (let ((deps (deps:create :trace-fn (lambda (fn-name) (declare (ignore fn-name)) NIL))))
                (clue:check-equal :expected NIL
                                  :actual (deps:trace-fn deps "bar"))))))


(defun test-untrace-fn ()
    (clue:suite "Untrace function"
        (clue:test "Success"
            (let ((deps (deps:create :untrace-fn (lambda (fn-name) (declare (ignore fn-name)) T))))
                (clue:check-equal :expected T
                                  :actual (deps:untrace-fn deps "bar"))))
        (clue:test "Fail"
            (let ((deps (deps:create :untrace-fn (lambda (fn-name) (declare (ignore fn-name)) NIL))))
                (clue:check-equal :expected NIL
                                  :actual (deps:untrace-fn deps "bar"))))))


(defun test-trace-pkg ()
    (clue:suite "Trace package"
        (clue:test "Success"
            (let ((deps (deps:create :trace-pkg (lambda (pkg-name) (declare (ignore pkg-name)) T))))
                (clue:check-equal :expected T
                                  :actual (deps:trace-pkg deps "bar"))))
        (clue:test "Fail"
            (let ((deps (deps:create :trace-pkg (lambda (pkg-name) (declare (ignore pkg-name)) NIL))))
                (clue:check-equal :expected NIL
                                  :actual (deps:trace-pkg deps "bar"))))))


(defun test-load-asdf ()
    (clue:test "Load ASDF system"
        (let ((deps (deps:create :list-all-asdf (lambda () (list 1 2)))))
            (clue:check-equal :expected T
                              :actual (deps:load-asdf-system deps)))))


(defun test-do-eval ()
    (clue:test "Eval"
        (let ((deps (deps:create :eval-fn (lambda (data) data))))
            (clue:check-equal :expected (list "foo")
                              :actual (deps:do-eval deps "foo")))))


(defun test-macro-expand ()
    (clue:test "Macro expand"
        (let ((deps (deps:create)))
            (deps:macro-expand deps "foo" "bar"))))


(defun test-macro-expand-1 ()
    (clue:test "Macro expand 1"
        (let ((deps (deps:create)))
            (deps:macro-expand-1 deps "foo" "bar"))))


(defun test-try-compile ()
    (clue:test "Try compile"
        (let ((deps (deps:create)))
            (deps:try-compile deps "foo"))))


(defun test-do-compile ()
    (clue:test "Do compile"
        (let ((deps (deps:create)))
            (deps:do-compile deps "foo"))))


(defun test-do-load ()
    (clue:test "Do load"
        (let ((deps (deps:create)))
            (deps:do-load deps "foo"))))


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
        (test-do-load)
        (test-trace-fn)
        (test-untrace-fn)
        (test-trace-pkg)))
