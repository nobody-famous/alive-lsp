(defpackage :alive/lsp/message/document/sem-tokens-full
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :request
             :req-params
             :text-document)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)
                      (:token :alive/parse/token)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:sem-types :alive/lsp/types/sem-tokens)))

(in-package :alive/lsp/message/document/sem-tokens-full)


(defclass request (message:request)
    ((message::method :initform "textDocument/semanticTokens/full")))


(defmethod print-object ((obj request) out)
    (format out "{method: \"~A\"; params: ~A}"
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (message:id a) (message:id b))
         (types:deep-equal-p (message:method-name a) (message:method-name b))
         (types:deep-equal-p (message:params a) (message:params b))))


(defun create-request (&key id (jsonrpc "2.0") params)
    (make-instance 'request
                   :id id
                   :jsonrpc jsonrpc
                   :params params))


(defclass req-params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)))


(defmethod print-object ((obj req-params) out)
    (format out "{text-document: ~A}"
            (text-document obj)))


(defmethod types:deep-equal-p ((a req-params) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (text-document a) (text-document b))))


(defun create-params (text-doc)
    (make-instance 'req-params
                   :text-document text-doc))


(defclass response (message:result-response)
    ())


(defclass sem-tokens ()
    ((data :accessor data
           :initform nil
           :initarg :data)))


(defun to-sem-array (sem-tokens)
    (loop :with line := 0
          :with col := 0
          :with out-list := nil

          :for token :in sem-tokens
          :for len := (- (sem-types:end-col token) (sem-types:start-col token))
          :for line-diff := (- (sem-types:line token) line)
          :for col-diff := (if (zerop line-diff)
                               (- (sem-types:start-col token) col)
                               (sem-types:start-col token)) :do

              (push line-diff out-list)
              (push col-diff out-list)
              (push len out-list)
              (push (sem-types:token-type token) out-list)
              (push 0 out-list)

              (setf line (sem-types:line token))
              (setf col (sem-types:start-col token))
          :finally (return (reverse out-list))))


(defun create-response (id sem-tokens)
    (make-instance 'response
                   :id id
                   :result (make-instance 'sem-tokens
                                          :data (to-sem-array sem-tokens))))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                              :jsonrpc jsonrpc
                                              :id id
                                              :params out-params)))))
