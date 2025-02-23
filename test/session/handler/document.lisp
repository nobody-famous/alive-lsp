(defpackage :alive/test/session/handler/document
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:doc :alive/session/handler/document)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/document)


(defparameter *msg-with-position* (list (cons :id 1)
                                        (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                            (cons :position (list (cons :line 5)
                                                                                  (cons :character 10)))))))


(defun test-completion ()
    (clue:suite "Completion Tests"
        (clue:test "Request"
            (let* ((state (state:create))
                   (resp (doc:new-completion state *msg-with-position*)))
                (clue:check-exists (gethash "result" resp))))

        (clue:test "Failure"
            (let* ((state (state:create)))
                (clue:expect-fail (lambda () (doc:new-completion state (list (cons :id 5)))))))))


(defun test-definition ()
    (clue:suite "Definition Tests"
        (clue:test "Request"
            (let* ((state (state:create))
                   (resp (doc:new-definition state *msg-with-position*)))
                (clue:check-exists (gethash "result" resp))))

        (clue:test "Failure"
            (let ((state (state:create)))
                (clue:expect-fail (lambda () (doc:new-definition state (list (cons :id 5)))))))))


(defun test-did-change ()
    (clue:suite "Did Change"
        (clue:test "No text"
            (let ((state (state:create)))
                (doc:did-change (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")))))))
                (clue:check-equal :expected nil
                                  :actual (state:new-get-file-text state "some/uri"))))

        (clue:test "Has text"
            (let ((state (state:create)))
                (doc:new-did-change state (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                                    (cons :content-changes (list (list (cons :text "Some text"))))))))
                (clue:check-equal :expected "Some text"
                                  :actual (state:new-get-file-text state "some/uri"))))))


(defun test-did-open ()
    (clue:suite "Did Open"
        (clue:test "No text"
            (let ((state (state:create)))
                (doc:new-did-open state (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")))))))
                (clue:check-equal :expected nil
                                  :actual (state:new-get-file-text state "some/uri"))))

        (clue:test "Has text"
            (let ((state (state:create)))
                (doc:new-did-open state (list (cons :params (list (cons :text-document (list (cons :uri "some/uri")
                                                                                             (cons :text "Some text")))))))
                (clue:check-equal :expected "Some text"
                                  :actual (state:new-get-file-text state "some/uri"))))))


(defun test-doc-symbols ()
    (clue:test "Document Symbols"
        (let ((state (state:create)))
            (clue:check-equal :expected T
                              :actual (hash-table-p (doc:new-doc-symbols state (list (cons :id 5)
                                                                                     (cons :params (list (cons :text-document (list (cons :uri "some/uri"))))))))))))


(defun test-hover ()
    (clue:test "Hover"
        (let ((state (state:create)))
            (clue:check-equal :expected T
                              :actual (hash-table-p (doc:new-hover state *msg-with-position*))))))


(defun test-on-type ()
    (clue:suite "Format On Type"
        (clue:test "No text"
            (let ((state (state:create)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p (doc:new-on-type state *msg-with-position*)))))

        (clue:test "With text"
            (let ((state (state:create)))
                (state:new-set-file-text state "some/uri" "foo")
                (clue:check-equal :expected T
                                  :actual (hash-table-p (doc:new-on-type state *msg-with-position*)))))))


(defun test-range-formatting ()
    (clue:test "Range Formatting"
        (let ((state (state:create)))
            (state:new-set-file-text state "uri" "Some test")
            (let* ((request (doc:new-formatting state (list (cons :id 5)
                                                            (cons :params (list (cons :text-document (list (cons :uri "some/uri"))))))))
                   (id (gethash "id" request))
                   (cb (state:new-get-sent-msg-callback state id)))

                (clue:check-equal :expected T
                                  :actual (hash-table-p (funcall cb (list (cons :id id)
                                                                          (cons :result (list (list (list))))))))))))


(defun test-selection ()
    (clue:suite "Selection"
        (clue:test "No forms"
            (let ((state (state:create)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p (doc:new-selection state (list (cons :id 1)
                                                                                       (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                                                                           (cons :positions (list (list (cons :line 5)
                                                                                                                                        (cons :character 10))))))))))))

        (clue:test "Have forms"
            (let ((state (state:create)))
                (state:new-set-file-text state "some/uri" "Some text")
                (clue:check-equal :expected T
                                  :actual (hash-table-p (doc:new-selection state (list (cons :id 1)
                                                                                       (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                                                                           (cons :positions (list (list (cons :line 5)
                                                                                                                                        (cons :character 10))))))))))))))


(defun test-sem-tokens ()
    (clue:suite "Semantic Tokens"
        (clue:test "Emtpy text"
            (let ((state (state:create)))
                (let* ((response (doc:new-sem-tokens state (list (cons :id 5)
                                                                 (cons :params (list (cons :text-document (list (cons :uri "some/uri"))))))))
                       (result (gethash "result" response)))
                    (clue:check-equal :expected nil
                                      :actual (gethash "data" result)))))

        (clue:test "Simple tokens"
            (let ((state (state:create)))
                (state:new-set-file-text state "some/uri" "#+n")
                (let* ((response (doc:new-sem-tokens state (list (cons :id 5)
                                                                 (cons :params (list (cons :text-document (list (cons :uri "some/uri"))))))))
                       (result (gethash "result" response)))
                    (clue:check-equal :expected (list 0 0 3 0 0)
                                      :actual (gethash "data" result)))))

        (clue:test "Multi-line token"
            (let ((state (state:create)))
                (state:new-set-file-text state "some/uri" (format nil "#| a bb~%~%ccc dddd~%|#"))
                (let* ((response (doc:new-sem-tokens state (list (cons :id 5)
                                                                 (cons :params (list (cons :text-document (list (cons :uri "some/uri"))))))))
                       (result (gethash "result" response)))
                    (clue:check-equal :expected (list 0 0 #xFFFF 0 0 1 0 #xFFFF 0 0 1 0 #xFFFF 0 0 1 0 2 0 0)
                                      :actual (gethash "data" result)))))))


(defun run-all ()
    (clue:suite "Document Handler Tests"
        (test-completion)
        (test-definition)
        (test-did-change)
        (test-did-open)
        (test-doc-symbols)
        (test-hover)
        (test-on-type)
        (test-range-formatting)
        (test-selection)
        (test-sem-tokens)))
