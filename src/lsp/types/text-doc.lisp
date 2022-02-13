(defpackage :alive/lsp/types/text-doc
    (:use :cl)
    (:export :from-wire
             :uri)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/lsp/types/text-doc)


(defclass text-document ()
    ((uri :accessor uri
          :initform nil
          :initarg :uri)
     (version :accessor version
              :initform nil
              :initarg :version)))


(defmethod print-object ((obj text-document) out)
    (format out "{uri: \"~A\"; version: ~A}"
            (uri obj)
            (version obj)))


(defmethod types:deep-equal-p ((a text-document) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (uri a) (uri b))
         (equalp (version a) (version b))))


(defun from-wire (fields)
    (labels ((add-field (id key value)
                  (cond ((eq key :uri) (setf (uri id) value))
                        ((eq key :version) (setf (version id) value)))))

        (loop :with id := (make-instance 'text-document)
              :for field :in fields :do
                  (add-field id (car field) (cdr field))
              :finally (return id))))