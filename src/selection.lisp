(defpackage :alive/selection
    (:use :cl)
    (:export :ranges)
    (:local-nicknames (:form :alive/parse/form)
                      (:pos :alive/position)
                      (:range :alive/range)))

(in-package :alive/selection)


(defun in-range (pos start end)
    (and (pos:less-or-equal start pos)
         (pos:less-than pos end)))


(defun find-form-for-pos (forms pos)
    (find-if (lambda (form)
                 (in-range pos (form:get-start form) (form:get-end form))) forms))


(defun create-node (parent form)
    (list (cons :range (range:create (form:get-start form) (form:get-end form)))
          (cons :parent parent)))


(defun get-range-tree (forms pos)
    (loop :with form := (find-form-for-pos forms pos)
          :with node := nil

          :while form

          :do (setf node (create-node node form))
              (setf form (if (form:get-kids form)
                             (find-form-for-pos (form:get-kids form) pos)
                             nil))

          :finally (return node)))


(defun ranges (forms pos-list)
    (mapcar (lambda (pos)
                (get-range-tree forms pos))
            pos-list))
