(defun %condition-message (condition)
  (string-trim #(#\newline #\space #\tab)
               (%%condition-message condition)))
