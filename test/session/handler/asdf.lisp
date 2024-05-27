(defpackage :alive/test/session/handler/asdf
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:asdf :alive/session/handler/asdf)
                      (:deps :alive/deps)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/asdf)


(defun test-list-all ()
    (clue:test "List All"
        (clue:expect-fail (lambda () (asdf:list-all (list (cons :id 5)))))
        (deps:with-deps (deps:create :list-all-asdf (lambda ()
                                                        (list 1)))
            (clue:check-exists (gethash "systems"
                                        (gethash "result" (asdf:list-all (list (cons :id 5)))))))))


(defun test-load-system ()
    (clue:test "Load System"
        (state:with-state (state:create)
            (deps:with-deps (deps:create :load-asdf-system (lambda (&key name stdin-fn stdout-fn stderr-fn force)
                                                               (declare (ignore name stdin-fn stdout-fn stderr-fn force))
                                                               T)
                                         :send-request (lambda (req)
                                                           (list (cons :id (gethash "id" req))))
                                         :send-msg (lambda (msg)
                                                       (declare (ignore msg))))
                (let ((thread (asdf:load-system (list (cons :id 5)))))
                    (bt:join-thread thread))))))


(defun run-all ()
    (clue:suite "ASDF Tests"
        (test-list-all)
        (test-load-system)))
