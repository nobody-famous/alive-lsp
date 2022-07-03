(in-package :clue)


(defun print-header (header)
    (let ((size (length header)))
        (format T "~%~A~%~v@{~A~:*~}~%"
            header
            size "-")))
