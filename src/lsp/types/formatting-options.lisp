(defpackage :alive/lsp/types/formatting-options
    (:use :cl)
    (:export :create-item
             :get-insert-spaces
             :get-tab-size
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/formatting-options)


(defclass options ()
        ((tab-size :accessor tab-size
                   :initform 2
                   :initarg :tab-size)
         (insert-spaces :accessor insert-spaces
                        :initform T
                        :initarg :insert-spaces)))


(defmethod print-object ((obj options) out)
    (format out "{tab-size: \"~A; insert-spaces: ~A\"}"
        (tab-size obj)
        (insert-spaces obj)))


(defun create-item (&key tab-size insert-spaces)
    (make-instance 'options
        :tab-size tab-size
        :insert-spaces insert-spaces))


(defun get-tab-size (obj)
    (when obj
          (tab-size obj)))


(defun get-insert-spaces (obj)
    (when obj
          (insert-spaces obj)))


(defun from-wire (results)
    (labels ((add-param (params key value)
                        (cond ((eq key :tab-size) (setf (tab-size params) value))
                              ((eq key :insert-spaces) (setf (insert-spaces params) value)))))

        (loop :with item := (make-instance 'options)
              :for param :in results :do
                  (add-param item (car param) (cdr param))
              :finally (return item))))