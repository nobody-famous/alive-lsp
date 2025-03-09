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
            (let ((state (state:create)))
                (clue:check-exists (gethash "result" (form-bounds:top-form state *default-msg*)))))

        (clue:test "With text"
            (let ((state (state:create)))
                (state:set-file-text state "some/uri" "foo")
                (clue:check-exists (gethash "result" (form-bounds:top-form state *default-msg*)))))))


(defun test-surrounding-form ()
    (clue:suite "Surrounding Form"
        (clue:test "No text"
            (let ((state (state:create)))
                (clue:check-exists (gethash "result" (form-bounds:surrounding-form state *default-msg*)))))

        (clue:test "With text"
            (let ((state (state:create)))
                (state:set-file-text state "some/uri" "(foo)")
                (clue:check-exists (gethash "result" (form-bounds:surrounding-form state *default-msg*)))))))


(defun run-all ()
    (clue:suite "Form Bounds Tests"
        (test-top-form)
        (test-surrounding-form)))
