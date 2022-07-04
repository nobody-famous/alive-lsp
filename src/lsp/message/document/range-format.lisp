(defpackage :alive/lsp/message/document/range-format
    (:use :cl)
    (:export :create-params
             :create-request
             :create-response
             :from-wire
             :text-document
             :range
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:edit :alive/text-edit)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:types :alive/types)))

(in-package :alive/lsp/message/document/range-format)


(defclass req-params ()
        ((text-document :accessor text-document
                        :initform nil
                        :initarg :text-document)
         (range :accessor range
                :initform nil
                :initarg :range)))


(defmethod print-object ((obj req-params) out)
    (format out "{text-document: ~A; range: ~A}"
        (text-document obj)
        (range obj)))


(defun create-params (&key text-document range)
    (make-instance 'req-params
        :text-document text-document
        :range range))


(defclass request (message:request)
        ((message::method :initform "textDocument/rangeFormatting")))


(defmethod print-object ((obj request) out)
    (format out "{method: \"~A\"; params: ~A}"
        (message:method-name obj)
        (message:params obj)))


(defun create-request (&key id jsonrpc params)
    (make-instance 'request
        :jsonrpc jsonrpc
        :id id
        :params params))


(defclass response (message:result-response)
        ())


(defclass text-edit ()
        ((range :accessor range
                :initform nil
                :initarg :range)
         (new-text :accessor new-text
                   :initform nil
                   :initarg :new-text)))


(defclass lsp-pos ()
        ((line :accessor line
               :initform nil
               :initarg :line)
         (character :accessor ch
                    :initform nil
                    :initarg :ch)))


(defclass lsp-range ()
        ((start :accessor start
                :initform nil
                :initarg :start)
         (end :accessor end
              :initform nil
              :initarg :end)))


(defun to-lsp-pos (pos)
    (make-instance 'lsp-pos
        :line (pos:line pos)
        :ch (pos:col pos)))


(defun to-lsp-range (range)
    (make-instance 'lsp-range
        :start (to-lsp-pos (range:start range))
        :end (to-lsp-pos (range:end range))))


(defun to-text-edits (edits)
    (if (and edits (< 0 (length edits)))
        (mapcar (lambda (edit)
                    (make-instance 'text-edit
                        :range (to-lsp-range (edit:range edit))
                        :new-text (edit:text edit)))
                edits)
        nil))


(defun create-response (id edits)
    (make-instance 'response
        :id id
        :result (to-text-edits edits)))


(defun from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                        (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:from-wire value)))
                              ((eq key :range) (setf (range out-params) (range:from-wire value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (create-request :jsonrpc jsonrpc
                                               :id id
                                               :params out-params)))))
