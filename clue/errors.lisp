(in-package :clue)


(define-condition test-failed (error)
        ((expected :accessor expected
                   :initform nil
                   :initarg :expected)
         (actual :accessor actual
                 :initform nil
                 :initarg :actual)
         (reason :accessor reason
                 :initform nil
                 :initarg :reason))
    (:report (lambda (condition stream)
                 (if (reason condition)
                     (format stream "~A"
                         (reason condition))
                     (format stream "Expected ~A, found ~A"
                         (expected condition)
                         (actual condition))))))
