(defpackage :alive/lsp/types/format-options
    (:use :cl)
    (:export :convert
             :create-item
             :get-indent-width
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/format-options)


(defclass format-options ()
        ((indent-width :accessor indent-width
                       :initform 2
                       :initarg :indent-width)))


(defmethod print-object ((obj format-options) out)
    (format out "{indent-width: \"~A\"}"
        (indent-width obj)))


(defun create-item (&key indent-width)
    (make-instance 'format-options
        :indent-width indent-width))


(defun get-indent-width (obj)
    (when obj
          (indent-width obj)))


(defun from-wire (results)
    (labels ((add-param (params key value)
                        (cond ((eq key :indent-width) (setf (indent-width params) value)))))

        (loop :with item := (make-instance 'format-options)
              :for result :in results :do
                  (loop :for param :in result :do
                            (add-param item (car param) (cdr param)))
              :finally (return item))))


(defun convert (opts)
    (mapcar (lambda (item)
                (cond ((eq :tab-size (car item)) (cons :indent-width (cdr item)))
                      (T item)))
            opts))