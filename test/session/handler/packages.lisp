(defpackage :alive/test/session/handler/packages
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:handler :alive/session/handler/packages)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/packages)


(defparameter *default-message* (list (cons :id 5)
                                      (cons :params (list (cons :text-document (list (cons :uri "some/uri")))
                                                          (cons :position (list (cons :line 0)
                                                                                (cons :character 1)))))))


(defun run-test (exp-pkg text)
    (state:with-state (state:create)
        (state:set-file-text "some/uri" text)
        (clue:check-equal :expected exp-pkg
                          :actual (gethash "package"
                                           (gethash "result" (handler:for-position *default-message*))))))


(defun test-for-pos ()
    (clue:suite "For Position"
        (clue:test "cl-user"
            (run-test "cl-user" "(+ 1 2)"))

        (clue:test "in-package"
            (run-test "alive/test/session/handler/packages" "(in-package :alive/test/session/handler/packages)"))))


(defun run-all ()
    (clue:suite "Packages Handler Tests"
        (test-for-pos)))
