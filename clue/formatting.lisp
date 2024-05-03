(in-package :clue)


(defun print-header (header)
    (let ((size (length header)))
        (format T "~%~A~%~A~%~v@{~A~:*~}~%"
            (format nil "~v@{~A~:*~}" size " ")
            header
            size "-")))


(defun print-footer (header results)
    (let ((size (length header)))
        (format T "~v@{~A~:*~}~%" size "-")
        (format T "Passed: ~A Failed: ~A~%" (test-results-passed results) (test-results-failed results))))
