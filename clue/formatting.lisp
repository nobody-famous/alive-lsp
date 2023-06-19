(in-package :clue)


(defun print-header (header)
    (let ((size (length header)))
        (format T "~%~A~%~A~%~v@{~A~:*~}~%"
            (format nil "~v@{~A~:*~}" size " ")
            header
            size "-")))
