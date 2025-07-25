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


(defun run-trace-test (text pos expected-fn)
    (let* ((state (state:create))
           (to-trace nil)
           (deps (deps:create :trace-fn (lambda (fn-name)
                                            (setf to-trace fn-name)
                                            T)))
           (msg (create-msg 5 "some/uri" pos))
           (resp (progn (state:set-file-text state "some/uri" text)
                        (handler:trace-fn deps state msg))))

        (clue:check-exists (gethash "result" resp))
        (clue:check-equal :expected expected-fn
                          :actual to-trace)))


(defun test-trace-fn ()
    (clue:suite "Trace Function"
        (clue:test "Function only"
            (run-trace-test "bar" (pos:create 0 2) "bar"))
        (clue:test "Pos in function"
            (run-trace-test "cl-user:bar" (pos:create 0 8) "bar"))
        (clue:test "Pos in package"
            (run-trace-test "cl-user:bar" (pos:create 0 2) "bar"))
        (clue:test "Pos in colons"
            (run-trace-test "cl-user::bar" (pos:create 0 8) "bar"))
        (clue:test "Colon without package"
            (run-trace-test ":bar" (pos:create 0 2) nil))
        (clue:test "Unknown package"
            (run-trace-test "foo:bar" (pos:create 0 5) nil))
        (clue:test "Not symbol"
            (run-trace-test "foo  bar" (pos:create 0 4) nil))))


(defun run-all ()
    (clue:suite "Traced Functions Tests"
        (test-list-all)))
