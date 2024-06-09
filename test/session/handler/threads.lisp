(defpackage :alive/test/session/handler/threads
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:state :alive/session/state)
                      (:threads :alive/session/handler/threads)))

(in-package :alive/test/session/handler/threads)


(defun test-list-all ()
    (clue:test "List all"
        (clue:expect-fail (lambda () (threads:list-all (list (cons :id 5)))))
        (state:with-state (state:create)
            (deps:with-deps (deps:create :list-all-threads (lambda ()
                                                               (list (list (cons :id 5))))
                                         :get-thread-id (lambda (thread)
                                                            (declare (ignore thread))
                                                            5))
                (clue:check-exists (gethash "result" (threads:list-all (list (cons :id 5)))))))))


(defun test-kill ()
    (clue:test "Kill"
        (clue:expect-fail (lambda () (threads:kill (list (cons :id 5)))))
        (state:with-state (state:create)
            (deps:with-deps (deps:create :kill-thread (lambda (id) (declare (ignore id)))
                                         :send-msg (lambda (msg) (declare (ignore msg)))
                                         :send-request (lambda (req)
                                                           (declare (ignore req))
                                                           (list (cons :id 5))))
                (clue:check-equal :expected T
                                  :actual (gethash "result" (threads:kill (list (cons :id 5)
                                                                                (cons :params (list (cons :id 10)))))))))))


(defun run-all ()
    (clue:suite "Threads Handler Tests"
        (test-list-all)
        (test-kill)))
