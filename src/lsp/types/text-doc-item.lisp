(defpackage :alive/lsp/types/text-doc-item
    (:use :cl)
    (:export :create-item
             :from-wire
             :text
             :uri)
    (:local-nicknames (:types :alive/types)))

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


(defmethod print-object ((obj text-document-item) out)
    (format out "{uri: \"~A\"; language-id: \"~A\"; version: \"~A\"; text: \"~A\"}"
        (uri obj)
        (language-id obj)
        (version obj)
        (text obj)))


(defun create-item (&key uri language-id version text)
    (make-instance 'text-document-item
        :uri uri
        :language-id language-id
        :version version
        :text text))


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