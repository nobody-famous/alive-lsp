(defpackage :alive/lsp/types/text-doc-item
    (:use :cl)
    (:export :from-wire
             :text
             :uri))

(in-package :alive/lsp/types/text-doc-item)


(defclass text-document-item ()
    ((uri :accessor uri
          :initform nil
          :initarg :uri)
     (language-id :accessor language-id
                  :initform nil
                  :initarg :language-id)
     (version :accessor version
              :initform nil
              :initarg :version)
     (text :accessor text
           :initform nil
           :initarg :text)))


(defun from-wire (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :uri) (setf (uri params) value))
                        ((eq key :language-id) (setf (language-id params) value))
                        ((eq key :text) (setf (text params) value))
                        ((eq key :version) (setf (version params) value))
                        (t (error (format nil "Unhandled text doc item key ~A" key))))))

        (loop :with item := (make-instance 'text-document-item)
              :for param :in params :do
                  (add-param item (car param) (cdr param))
              :finally (return item))))