(defpackage :alive/selection
    (:use :cl)
    (:export :ranges)
    (:local-nicknames (:pos :alive/position)
                      (:range :alive/range)))

(in-package :alive/selection)


(defun in-range (pos start end)
    (and (pos:less-or-equal start pos)
         (pos:less-than pos end)))


(defun ranges (forms pos-list)
    (loop :with start := nil
          :with end := nil
          :with out := nil

          :for form :in forms
          :do (let ((form-start (gethash "start" form))
                    (form-end (gethash "end" form)))

                  (unless start
                      (setf start form-start))
                  (setf end form-end)

                  (loop :for pos :in pos-list
                        :do (when (in-range pos form-start form-end)
                                  (push (range:create form-start form-end) out)
                                  (loop :for kid :in (gethash "kids" form)
                                        :do (when (in-range pos (gethash "start" kid) (gethash "end" kid))
                                                  (setf out (append out (ranges (list kid) (list pos)))))))))

          :finally (progn (push (range:create start end) out)
                          (return out))))
