(in-package :clue)


(define-condition test-failed (error)
        ((expected :accessor expected
                   :initform nil
                   :initarg :expected)
         (actual :accessor actual
                 :initform nil
                 :initarg :actual))
    (:report (lambda (condition stream)
                 (format stream "Expected ~A, found ~A"
                     (expected condition)
                     (actual condition)))))
