"Test string \"quoted\"
and multiline"
bar


(defun foo ()
    (if (zerop 5)
        (if (zerop 5) 3 4)
        1)

    (format "Foo Called ~A~%")
    (format "Foo Called ~A~%"))
