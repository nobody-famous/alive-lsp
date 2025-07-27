(defpackage :alive/test/session/handler/traced-fns
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:deps :alive/deps)
                      (:handler :alive/session/handler/traced-fns)
                      (:pos :alive/position)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/traced-fns)


(defun create-trace-fn-msg (id file pos)
    (list (cons :id id)
          (cons :params (list (cons :position pos)
                              (cons :text-document (list (cons :uri file)))))))


(defun create-trace-pkg-msg (id pkg-name)
    (list (cons :id id)
          (cons :params (list (cons :package pkg-name)))))


(defun test-list-all ()
    (clue:test "List All"
        (let ((result (gethash "result" (handler:list-all (deps:create :list-all-traced (lambda () (list (cons :a "b"))))
                                                          (list (cons :id 5))))))
            (clue:check-exists (gethash "traced" result)))))


(defmacro run-test (fn text pos expected-fn)
    `(let* ((state (state:create))
            (to-trace nil)
            (resp nil)
            (deps (deps:create ,(intern (concatenate 'string ":" (symbol-name fn)) :keyword) (lambda (fn-name)
                                                                                                 (setf to-trace fn-name)
                                                                                                 T)
                               :send-msg (lambda (msg)
                                             (setf resp msg)
                                             nil)))
            (msg (`create-trace-fn-msg 5 "some/uri" ,pos)))

         (state:set-file-text state "some/uri" ,text)
         (funcall (symbol-function (intern (string ,fn) "HANDLER")) deps state msg)

         (clue:check-exists (gethash "result" resp))
         (clue:check-equal :expected ,expected-fn
                           :actual to-trace)))


(defun run-trace-test (text pos expected-fn)
    (let* ((state (state:create))
           (to-trace nil)
           (resp nil)
           (deps (deps:create :trace-fn (lambda (fn-name)
                                            (setf to-trace fn-name)
                                            T)
                              :send-msg (lambda (msg)
                                            (setf resp msg)
                                            nil)))
           (msg (create-trace-fn-msg 5 "some/uri" pos)))

        (state:set-file-text state "some/uri" text)
        (handler:trace-fn deps state msg)

        (clue:check-exists (gethash "result" resp))
        (clue:check-equal :expected expected-fn
                          :actual to-trace)))


(defun run-untrace-test (text pos expected-fn)
    (let* ((state (state:create))
           (to-trace nil)
           (resp nil)
           (deps (deps:create :untrace-fn (lambda (fn-name)
                                              (setf to-trace fn-name)
                                              T)
                              :send-msg (lambda (msg)
                                            (setf resp msg)
                                            nil)))
           (msg (create-trace-fn-msg 5 "some/uri" pos)))

        (state:set-file-text state "some/uri" text)
        (handler:untrace-fn deps state msg)

        (clue:check-exists (gethash "result" resp))
        (clue:check-equal :expected expected-fn
                          :actual to-trace)))


(defun test-trace-fn ()
    (clue:suite "Trace Function"
        (clue:test "Function only"
            (run-trace-test "bar" (pos:create 0 2) "bar"))
        (clue:test "Pos in function"
            (run-trace-test "cl-user:bar" (pos:create 0 8) "cl-user:bar"))
        (clue:test "Pos in package"
            (run-trace-test "cl-user:bar" (pos:create 0 2) "cl-user:bar"))
        (clue:test "Pos in colons"
            (run-trace-test "cl-user::bar" (pos:create 0 8) "cl-user::bar"))
        (clue:test "Colon without package"
            (run-trace-test ":bar" (pos:create 0 2) nil))
        (clue:test "Unknown package"
            (run-trace-test "foo:bar" (pos:create 0 5) "foo:bar"))
        (clue:test "Not symbol"
            (run-trace-test "foo  bar" (pos:create 0 4) nil))))


(defun test-untrace-fn ()
    (clue:suite "Untrace Function"
        (clue:test "Function only"
            (run-untrace-test "bar" (pos:create 0 2) "bar"))
        (clue:test "Pos in function"
            (run-untrace-test "cl-user:bar" (pos:create 0 8) "cl-user:bar"))
        (clue:test "Pos in package"
            (run-untrace-test "cl-user:bar" (pos:create 0 2) "cl-user:bar"))
        (clue:test "Pos in colons"
            (run-untrace-test "cl-user::bar" (pos:create 0 8) "cl-user::bar"))
        (clue:test "Colon without package"
            (run-untrace-test ":bar" (pos:create 0 2) nil))
        (clue:test "Unknown package"
            (run-untrace-test "foo:bar" (pos:create 0 5) "foo:bar"))
        (clue:test "Not symbol"
            (run-untrace-test "foo  bar" (pos:create 0 4) nil))))


(defun test-trace-pkg ()
    (clue:suite "Trace Package"
        (clue:test "Success"
            (let* ((to-trace nil)
                   (resp nil)
                   (deps (deps:create :trace-pkg (lambda (pkg-name)
                                                     (setf to-trace pkg-name)
                                                     T)
                                      :send-msg (lambda (msg)
                                                    (setf resp msg)
                                                    nil)))
                   (msg (create-trace-pkg-msg 5 "foo")))

                (handler:trace-pkg deps msg)
                (clue:check-exists (gethash "result" resp))
                (clue:check-equal :expected "foo"
                                  :actual to-trace)))))


(defun run-all ()
    (clue:suite "Traced Functions Tests"
        (test-list-all)
        (test-trace-fn)
        (test-untrace-fn)
        (test-trace-pkg)))
