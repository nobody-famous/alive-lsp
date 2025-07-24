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
                   (to-trace nil)
                   (deps (deps:create :trace-fn (lambda (fn-name)
                                                    (setf to-trace fn-name)
                                                    T)))
                   (msg (create-msg 5 "some/uri" (pos:create 0 8)))
                   (resp (progn (state:set-file-text state "some/uri" "cl-user:bar")
                                (handler:trace-fn deps state msg))))

                (clue:check-exists (gethash "result" resp))
                (clue:check-equal :expected "bar"
                                  :actual to-trace)))))


(defun run-all ()
    (clue:suite "Traced Functions Tests"
        (test-list-all)))
