(defpackage :alive/debugger
    (:use :cl)
    (:export :get-frame-loc)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:form :alive/parse/form)))

(in-package :alive/debugger)


(defun find-form (cur-count target forms)
    nil)


(defun get-frame-loc (stream frame)
    (let* ((top-ndx (gethash "topForm" frame))
           (form-num (gethash "formNumber" frame))
           (forms (when stream
                        (forms:from-stream stream)))
           (top-form (when (and top-ndx forms)
                           (nth top-ndx forms))))

        (format T "loc ~A ~A ~A~%" top-ndx form-num top-form)
        (when top-form
              (let ((kids (gethash "kids" top-form)))
                  (format T "KIDS ~A ~A ~A~%"
                      (length kids)
                      (gethash "start" top-form)
                      form-num)
                  (loop :for kid :in kids
                        :do (format T "  ~A~%" (form:to-string kid)))))))
