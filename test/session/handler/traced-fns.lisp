(defpackage :alive/test/session/handler/traced-fns
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:handler :alive/session/handler/traced-fns)
                      (:pos :alive/position)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/traced-fns)


(defun create-msg (id file pos)
    (list (cons :id id)
          (cons :params (list (cons :position pos)
                              (cons :text-document (list (cons :uri file)))))))


(defun test-list-all ()
    (clue:test "List All"
        (let ((result (gethash "result" (handler:list-all (deps:create :list-all-traced (lambda () (list (cons :a "b"))))
                                                          (list (cons :id 5))))))
            (clue:check-exists (gethash "traced" result)))))


(defun test-trace-fn ()
    (clue:suite "Trace Function"
        (clue:test "Simple package and function"
            (let* ((state (state:create))
                   (deps (deps:create))
                   (msg (create-msg 5 "some/uri" (pos:create 0 2)))
                   (resp (progn (state:set-file-text state "some/uri" "foo:bar")
                                (handler:trace-fn deps state msg))))

                (alive/test/utils:print-hash-table "***** RESP" resp)))))


(defun run-all ()
    (clue:suite "Traced Functions Tests"
        (test-list-all)))
