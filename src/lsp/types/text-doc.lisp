(defpackage :alive/lsp/types/text-doc
    (:use :cl)
    (:export :create
             :from-wire
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


(defun create (&key uri version)
    (make-instance 'text-document
        :uri uri
        :version version))


(defun from-wire (fields)
    (labels ((add-field (id key value)
                        (cond ((eq key :uri) (setf (uri id) value))
                              ((eq key :version) (setf (version id) value)))))

        (loop :with id := (make-instance 'text-document)

              :for field :in fields :do
                  (add-field id (car field) (cdr field))

              :finally (return id))))