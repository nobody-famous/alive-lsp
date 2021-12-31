(defpackage :alive/lsp/types/text-doc
    (:use :cl)
    (:export :id-from-wire
             :uri))

(in-package :alive/lsp/types/text-doc)


(defclass identifier ()
    ((uri :accessor uri
          :initform nil
          :initarg :uri)))


(defun id-from-wire (fields)
    (labels ((add-field (id key value)
                  (cond ((eq key :uri) (setf (uri id) value)))))

        (loop :with id := (make-instance 'identifier)
              :for field :in fields :do
                  (add-field id (car field) (cdr field))
              :finally (return id))))