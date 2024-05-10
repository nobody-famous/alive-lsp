(defpackage :alive/test/session/handler/document
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:doc :alive/session/handler/document)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/document)


(defun test-completion ()
    (clue:suite "Completion Tests"
        (clue:test "Request"
            (state:with-state (state:create)
                (let ((resp (doc:completion (list (cons :id 1)
                                                  (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                                      (cons :position (list (cons :line 5)
                                                                                            (cons :character 10)))))))))
                    (clue:check-exists (gethash "result" resp)))))

        (clue:test "Failure"
            (clue:expect-fail (lambda () (doc:completion (list (cons :id 5))))))))


(defun test-definition ()
    (clue:suite "Definition Tests"
        (clue:test "Request"
            (state:with-state (state:create)
                (let ((resp (doc:definition (list (cons :id 1)
                                                  (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                                      (cons :position (list (cons :line 5)
                                                                                            (cons :character 10)))))))))
                    (clue:check-exists (gethash "result" resp)))))

        (clue:test "Failure"
            (clue:expect-fail (lambda () (doc:definition (list (cons :id 5))))))))


(defun run-all ()
    (clue:suite "Document Handler Tests"
        (test-completion)
        (test-definition)))
