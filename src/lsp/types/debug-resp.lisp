(defpackage :alive/lsp/types/debug-resp
    (:use :cl)
    (:export :create-options
             :get-index
             :from-wire)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/debug-resp)


(defclass debug-resp ()
        ((index :accessor index
                :initform nil
                :initarg :index)))


(defmethod print-object ((obj debug-resp) out)
    (format out "{index: \"~A\"}"
        (index obj)))


(defmethod types:deep-equal-p ((a debug-resp) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (index a) (index b))))


(defun create-item (&key index)
    (make-instance 'debug-resp
        :index index))


(defun get-index (obj)
    (when obj
          (index obj)))


(defun from-wire (results)
    (labels ((add-param (params key value)
                        (cond ((eq key :index) (setf (index params) value)))))

        (loop :with item := (make-instance 'debug-resp)
              :for result :in results :do

                  (add-param item (car result) (cdr result))

              :finally (return item))))