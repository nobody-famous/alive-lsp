(defpackage :alive/selection
    (:use :cl)
    (:export :ranges)
    (:local-nicknames (:pos :alive/position)
                      (:range :alive/range)))

(in-package :alive/selection)


(defun in-range (pos start end)
    (and (pos:less-or-equal start pos)
         (pos:less-than pos end)))


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


(defstruct pos
    line
    col)


(declaim (ftype (function (cons))))
(defun position-p (data)
    (let ((line (first data))
          (char (second data)))
        (and (eq :line (car line))
             (numberp (cdr line))
             (eq :character (car char))
             (numberp (cdr char)))))


(defun list-of-position-p (data)
    (and (consp data)
         (every #'position-p data)))


(deftype list-of-position ()
    `(satisfies list-of-position-p))


(declaim (ftype (function (cons list-of-position)) ranges))
(defun ranges (forms pos-list)
    (mapcar (lambda (pos)
                (ranges-for-pos forms pos))
            pos-list))
