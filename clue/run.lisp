(in-package :clue)


(defmacro suite (label &body body)
    `(progn (print-header ,label)
            ,@body))


(defmacro test (label &body body)
    `(let ((result (handler-case

                           (progn ,@body)

                       (error (c) (format nil "~A" c)))))

         (format T "~A [~A]~%" ,label (if result
                                          (format nil "FAILED: ~A" result)
                                          "OK"))))
