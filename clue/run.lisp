(in-package :clue)


(defparameter *test-indent* 0)


(defstruct test-results
    (passed 0 :type fixnum)
    (failed 0 :type fixnum))


(defun indent ()
    (format nil "~v@{~A~:*~}" (- *test-indent* 1) "|  "))


(defmacro suite (label &body body)
    (let ((out (gensym)))
        `(let ((,out (make-test-results))
               (*test-indent* (+ *test-indent* 1)))
             (print-header ,label)
             ,@(loop :for expr :in body
                     :collect `(let ((expr-result ,expr))
                                   (when (test-results-p expr-result)
                                         (incf (test-results-passed ,out) (test-results-passed expr-result))
                                         (incf (test-results-failed ,out) (test-results-failed expr-result)))) :into exprs
                     :finally (return exprs))
             (print-footer ,label ,out)
             ,out)))


(defmacro test (label &body body)
    `(let ((*test-indent* (+ *test-indent* 1))
           (result (handler-case
                           (progn ,@body)
                       (error (c) (format nil "~A" c)))))

         (format T "~A[~A] ~A~%"
             (indent)
             (if result
                 (format nil "FAILED: ~A" result)
                 "OK")
             ,label)
         (make-test-results :passed (if result 0 1) :failed (if result 1 0))))
