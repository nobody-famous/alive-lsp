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


(defun test-did-change ()
    (clue:suite "Did Change"
        (clue:test "No text"
            (state:with-state (state:create)
                (doc:did-change (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")))))))
                (clue:check-equal :expected nil
                                  :actual (state:get-file-text "some/uri"))))

        (clue:test "Has text"
            (state:with-state (state:create)
                (doc:did-change (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                          (cons :content-changes (list (list (cons :text "Some text"))))))))
                (clue:check-equal :expected "Some text"
                                  :actual (state:get-file-text "some/uri"))))))


(defun test-did-open ()
    (clue:suite "Did Open"
        (clue:test "No text"
            (state:with-state (state:create)
                (doc:did-open (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")))))))
                (clue:check-equal :expected nil
                                  :actual (state:get-file-text "some/uri"))))

        (clue:test "Has text"
            (state:with-state (state:create)
                (doc:did-open (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")
                                                                                   (cons :text "Some text")))))))
                (clue:check-equal :expected "Some text"
                                  :actual (state:get-file-text "some/uri"))))))


(defun test-doc-symbols ()
    (clue:test "Document Symbols"
        (state:with-state (state:create)
            (clue:check-equal :expected T
                              :actual (hash-table-p (doc:doc-symbols (list (cons :id 5)
                                                                           (cons :params (list (cons :text-document (list (cons :uri "some/uri"))))))))))))


(defun test-hover ()
    (clue:test "Hover"
        (state:with-state (state:create)
            (clue:check-equal :expected T
                              :actual (hash-table-p (doc:hover (list (cons :id 1)
                                                                     (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                                                         (cons :position (list (cons :line 5)
                                                                                                               (cons :character 10))))))))))))


(defun run-all ()
    (clue:suite "Document Handler Tests"
        (test-completion)
        (test-definition)
        (test-did-change)
        (test-did-open)
        (test-doc-symbols)
        (test-hover)))
