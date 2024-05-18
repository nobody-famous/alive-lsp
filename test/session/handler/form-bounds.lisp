(defpackage :alive/test/session/handler/form-bounds
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:form-bounds :alive/session/handler/form-bounds)
                      (:state :alive/session/state)))

(in-package :alive/test/session/handler/form-bounds)


(defparameter *default-msg* (list (cons :id 5)
                                  (cons :params (list (cons :position (list (cons :line 0)
                                                                            (cons :character 2)))
                                                      (cons :text-document (list (cons :uri "some/uri")))))))


(defun test-top-form ()
    (clue:suite "Top Form"
        (clue:test "No text"
            (clue:expect-fail (lambda () (form-bounds:top-form *default-msg*)))
            (state:with-state (state:create)
                (clue:check-exists (gethash "result" (form-bounds:top-form *default-msg*)))))

        (clue:test "With text"
            (clue:expect-fail (lambda () (form-bounds:top-form *default-msg*)))
            (state:with-state (state:create)
                (state:set-file-text "some/uri" "foo")
                (clue:check-exists (gethash "result" (form-bounds:top-form *default-msg*)))))))


(defun run-all ()
    (clue:suite "Form Bounds Tests"
        (test-top-form)))
