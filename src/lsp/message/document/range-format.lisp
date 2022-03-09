(defpackage :alive/lsp/message/document/range-format
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:types :alive/types)))

(in-package :alive/lsp/message/document/range-format)


(defclass req-params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)
     (start :accessor start
            :initform nil
            :initarg :start)
     (end :accessor end
          :initform nil
          :initarg :end)))


(defmethod print-object ((obj req-params) out)
    (format out "{text-document: ~A; start: ~A; end: ~A}"
            (text-document obj)
            (start obj)
            (end obj)))


(defmethod types:deep-equal-p ((a req-params) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (text-document a) (text-document b))
         (types:deep-equal-p (start a) (start b))
         (types:deep-equal-p (end a) (end b))))


(defun create-params (&key text-document start end)
    (make-instance 'req-params
                   :text-document text-document
                   :start start
                   :end end))


(defclass request (message:request)
    ((message::method :initform "textDocument/rangeFormatting")))


(defmethod print-object ((obj request) out)
    (format out "{method: \"~A\"; params: ~A}"
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (message:id a) (message:id b))
         (types:deep-equal-p (message:method-name a) (message:method-name b))
         (types:deep-equal-p (message:params a) (message:params b))))


(defun create-request (&key id jsonrpc params)
    (make-instance 'request
                   :jsonrpc jsonrpc
                   :id id
                   :params params))


(defun create-response ()
    nil)


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value)))
                        ((eq key :start) (setf (start out-params) (pos:from-wire value)))
                        ((eq key :end) (setf (end out-params) (pos:from-wire value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))
