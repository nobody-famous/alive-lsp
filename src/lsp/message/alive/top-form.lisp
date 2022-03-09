(defpackage :alive/lsp/message/alive/top-form
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :offset
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
     (offset :accessor offset
             :initform nil
             :initarg :offset)))


(defmethod print-object ((obj params) out)
    (format out "{text-document: ~A; position: ~A}"
            (text-document obj)
            (offset obj)))


(defmethod types:deep-equal-p ((a params) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (text-document a) (text-document b))
         (types:deep-equal-p (offset a) (offset b))))


(defun create-params (&key text-document offset)
    (make-instance 'params
                   :text-document text-document
                   :offset offset))


(defclass response (message:result-response)
    ())


(defmethod print-object ((obj response) out)
    (format out "{id: ~A; result: ~A}"
            (message:id obj)
            (message:result obj)))


(defclass response-body ()
    ((start :accessor start
            :initform nil
            :initarg :start)
     (end :accessor end
          :initform nil
          :initarg :end)))


(defmethod print-object ((obj response-body) out)
    (format out "{start: ~A; end: ~A}"
            (start obj)
            (end obj)))


(defun create-response (&key id start end)
    (make-instance 'response
                   :id id
                   :result (make-instance 'response-body
                                          :start start
                                          :end end)))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value)))
                        ((eq key :offset) (setf (offset out-params) value)))))

        (loop :with out-params := (make-instance 'params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                              :jsonrpc jsonrpc
                                              :id id
                                              :params out-params)))))
