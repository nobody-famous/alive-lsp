(defpackage :alive/debugger
    (:use :cl)
    (:export :get-frame-loc)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:form :alive/parse/form)
                      (:types :alive/types)))

(in-package :alive/debugger)


(defun find-form (cur-count target forms)
    (loop :with open-parens := (remove-if-not (lambda (form)
                                                  (= types:*open-paren* (gethash "formType" form)))
                                       forms)
          :with ndx := cur-count

          :for form :in open-parens

          :do (when (= ndx target)
                    (return-from find-form (values nil form)))

              (multiple-value-bind (new-ndx found)
                      (find-form (+ 1 ndx) target (gethash "kids" form))
                  (if found
                      (return-from find-form (values nil found))
                      (setf ndx new-ndx)))

          :finally (return (values ndx nil))))


(defun get-frame-loc (stream frame)
    (let* ((top-ndx (when frame
                          (gethash "topForm" frame)))
           (form-num (when frame
                           (gethash "formNumber" frame)))
           (forms (when stream
                        (forms:from-stream stream)))
           (top-form (when (and top-ndx forms)
                           (forms:get-nth-form forms top-ndx))))

        (when top-form
              (multiple-value-bind (ndx found)
                      (find-form 0 form-num (gethash "kids" top-form))
                  (declare (ignore ndx))

                  (when found
                        (gethash "start" found))))))
