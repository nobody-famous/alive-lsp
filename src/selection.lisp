(defpackage :alive/selection
    (:use :cl)
    (:export :ranges)
    (:local-nicknames (:pos :alive/position)
                      (:range :alive/range)))

(in-package :alive/selection)


(declaim (ftype (function (pos:text-position pos:text-position pos:text-position) boolean) in-range))
(defun in-range (pos start end)
    (and (pos:less-or-equal start pos)
         (pos:less-than pos end)))


(declaim (ftype (function (cons pos:text-position) (or boolean hash-table)) find-form-for-pos))
(defun find-form-for-pos (forms pos)
    (find-if (lambda (form)
                 (in-range pos (gethash "start" form) (gethash "end" form))) forms))


(declaim (ftype (function ((or boolean cons) (or boolean hash-table)) cons) create-node))
(defun create-node (parent form)
    (list (cons :range (range:create (gethash "start" form) (gethash "end" form)))
          (cons :parent parent)))


(declaim (ftype (function (cons pos:text-position)) get-range-tree))
(defun get-range-tree (forms pos)
    (loop :with form := (find-form-for-pos forms pos)
          :with node := nil

          :while form

          :do (setf node (create-node node form))
              (setf form (if (gethash "kids" form)
                             (find-form-for-pos (gethash "kids" form) pos)
                             nil))

          :finally (return node)))


(defun list-of-position-p (data)
    (and (consp data)
         (every #'pos:position-p data)))


(deftype list-of-position ()
    `(satisfies list-of-position-p))


(declaim (ftype (function (cons list-of-position)) ranges))
(defun ranges (forms pos-list)
    (mapcar (lambda (pos)
                (get-range-tree forms pos))
            pos-list))
