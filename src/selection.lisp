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


(declaim (ftype (function (cons pos:text-position) cons) ranges-for-pos))
(defun ranges-for-pos (forms pos)
    (loop :with start := nil
          :with end := nil
          :with out := nil

          :for form :in forms
          :do (let ((form-start (gethash "start" form))
                    (form-end (gethash "end" form)))

                  (unless start
                      (setf start form-start))
                  (setf end form-end)

                  (when (in-range pos form-start form-end)
                        (push (range:create form-start form-end) out)
                        (loop :for kid :in (gethash "kids" form)
                              :do (when (in-range pos (gethash "start" kid) (gethash "end" kid))
                                        (setf out (append out
                                                      (ranges-for-pos (list kid) pos)))))))

          :finally (progn (push (range:create start end) out)
                          (return out))))


(declaim (ftype (function (cons pos:text-position) (or T cons)) find-form-for-pos))
(defun find-form-for-pos (forms pos)
    (first (remove-if-not (lambda (form)
                              (in-range pos (gethash "start" form) (gethash "end" form)))
                   forms)))


(declaim (ftype (function ((or T cons) hash-table)) create-node))
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
                (ranges-for-pos forms pos))
            pos-list))
