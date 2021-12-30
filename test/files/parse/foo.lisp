; Some comment

"Test string \"quoted\"
and multiline"

#+(or sbcl) bar:baz

#\)
#\(

(defun foo ()
    (if (zerop 5)
        (if (zerop 5) 3 4) ;; End of line comment
        1)

    (format "Foo Called ~A~%")
    (format "Foo Called ~A~%"))
