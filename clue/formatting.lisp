(in-package :clue)


(defun print-header (header)
    (let ((size (length header)))
        (format T "~A~A~%~A~v@{~A~:*~}~%"
            (indent)
            header
            (indent)
            size "-")))


(defun print-footer (header results)
    (let ((size (length header)))
        (format T "~A~v@{~A~:*~}~%" (indent) size "-")
        (format T "~APassed: ~A Failed: ~A~%"
            (indent)
            (test-results-passed results)
            (test-results-failed results))))
