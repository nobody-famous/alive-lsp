(defpackage :alive/debugger
    (:use :cl)
    (:export :get-frame-loc)
    (:local-nicknames (:forms :alive/parse/forms)))

(in-package :alive/debugger)


(defun find-form (cur-count target forms)
    nil)


(defun get-frame-loc (stream frame)
    (let* ((top-ndx (gethash "topForm" frame))
           (form-num (gethash "formNumber" frame))
           (forms (when stream
                        (forms:from-stream stream)))
           (top-form (when forms
                           (nth top-ndx forms))))

        (when top-form
              (format T "KIDS ~A ~A ~A~%"
                  (type-of (gethash "kids" (first forms)))
                  (gethash "start" top-form)
                  form-num))))
