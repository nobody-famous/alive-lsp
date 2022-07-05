(defpackage :alive/lsp/message/workspace/config
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :pos
             :request
             :text-document)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:config-item :alive/lsp/types/config-item)
                      (:types :alive/types)))

(in-package :alive/lsp/message/workspace/config)


(defclass request (message:request)
        ((message::method :initform "workspace/configuration")))


(defmethod print-object ((obj request) out)
    (format out "{method: \"~A\"; params: ~A}"
        (message:method-name obj)
        (message:params obj)))


(defclass response-body ()
        ((is-incomplete :accessor is-incomplete
                        :initform T
                        :initarg :is-incomplete)
         (items :accessor items
                :initform nil
                :initarg :items)))


(defclass response (message:result-response)
        ())


(defun create-response (&key id items)
    (make-instance 'response
        :id id
        :result (make-instance 'response-body :items items)))


(defun create-request (&key id (jsonrpc "2.0") params)
    (make-instance 'request
        :id id
        :jsonrpc jsonrpc
        :params params))


(defclass req-params ()
        ((items :accessor items
                :initform nil
                :initarg :items)))


(defmethod print-object ((obj req-params) out)
    (format out "{items: ~A}"
        (items obj)))


(defun create-params (&key items)
    (make-instance 'req-params
        :items items))


(defun parse-items (value)
    (loop :with items := '()
          :for item :in value :do
              (push (config-item:from-wire item) items)
          :finally (return items)))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                        (cond ((eq key :items) (setf (items out-params) (parse-items value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                   :jsonrpc jsonrpc
                                   :id id
                                   :params out-params)))))
