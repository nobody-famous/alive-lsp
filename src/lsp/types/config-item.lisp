(defpackage :alive/lsp/types/config-item
    (:use :cl)
    (:export :create-item
             :from-wire
             :text
             :uri)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/config-item)


(defclass config-item ()
    ((scope-uri :accessor scope-uri
                :initform nil
                :initarg :scope-uri)
     (section :accessor section
              :initform nil
              :initarg :section)))


(defmethod print-object ((obj config-item) out)
    (format out "{scope-uri: \"~A\"; section: \"~A\"}"
            (scope-uri obj)
            (section obj)))


(defmethod types:deep-equal-p ((a config-item) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (scope-uri a) (scope-uri b))
         (types:deep-equal-p (section a) (section b))))


(defun create-item (&key scope-uri section)
    (make-instance 'config-item
                   :scope-uri scope-uri
                   :section section))


(defun from-wire (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :scope-uri) (setf (scope-uri params) value))
                        ((eq key :section) (setf (section params) value)))))

        (loop :with item := (make-instance 'config-item)
              :for param :in params :do
                  (add-param item (car param) (cdr param))
              :finally (return item))))