(defpackage :alive/lsp/message/alive/top-form
    (:use :cl)
    (:export :create-params
             :create-request
             :from-wire
             :pos
             :request
             :text-document)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/top-form)


(defclass request (message:request)
    ((message::method :initform "$/alive/topFormBounds")))


(defmethod print-object ((obj request) out)
    (format out "{id: ~A; method: ~A; params: ~A}"
            (message:id obj)
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (message:id a) (message:id b))
         (types:deep-equal-p (message:params a) (message:params b))))


(defun create-request (&key jsonrpc id params)
    (make-instance 'request
                   :jsonrpc jsonrpc
                   :id id
                   :params params))


(defclass params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)
     (pos :accessor pos
          :initform nil
          :initarg :pos)))


(defmethod print-object ((obj params) out)
    (format out "{text-document: ~A; position: ~A}"
            (text-document obj)
            (pos obj)))


(defmethod types:deep-equal-p ((a params) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (text-document a) (text-document b))
         (types:deep-equal-p (pos a) (pos b))))


(defun create-params (&key text-document pos)
    (make-instance 'params
                   :text-document text-document
                   :pos pos))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value)))
                        ((eq key :position) (setf (pos out-params) (pos:from-wire value))))))

        (loop :with out-params := (make-instance 'params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                              :jsonrpc jsonrpc
                                              :id id
                                              :params out-params)))))
