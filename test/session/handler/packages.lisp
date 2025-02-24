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
    (let ((state (state:create)))
        (state:set-file-text state "some/uri" text)
        (clue:check-equal :expected exp-pkg
                          :actual (gethash "package"
                                           (gethash "result" (handler:new-for-position state *default-message*))))))


(defun test-for-pos ()
    (clue:suite "For Position"
        (clue:test "cl-user"
            (run-test "cl-user" "(+ 1 2)"))

        (clue:test "in-package"
            (run-test "alive/test/session/handler/packages" "(in-package :alive/test/session/handler/packages)"))))


(defun test-list-all ()
    (clue:test "List All"
        (let ((result (gethash "result" (handler:list-all (list (cons :id 5))))))
            (clue:check-exists (gethash "packages" result)))))


(defun test-remove ()
    (clue:test "Remove"
        (clue:check-exists (gethash "result" (handler:remove-pkg (list (cons :id 5)))))))


(defun run-all ()
    (clue:suite "Packages Handler Tests"
        (test-for-pos)
        (test-list-all)
        (test-remove)))
