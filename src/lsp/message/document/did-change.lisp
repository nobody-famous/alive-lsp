(defpackage :alive/lsp/message/document/did-change
    (:use :cl)
    (:export :did-change
             :from-wire
             :get-text
             :get-uri)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:types :alive/types)))

(in-package :alive/lsp/message/document/did-change)


(defclass did-change (message:notification)
    ((message::method :initform "textDocument/didChange")))


(defmethod print-object ((obj did-change) out)
    (format out "{method: \"~A\"; params: ~A}"
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a did-change) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (message:method-name a) (message:method-name b))
         (types:deep-equal-p (message:params a) (message:params b))))


(defclass params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)
     (content-changes :accessor content-changes
                      :initform nil
                      :initarg :content-changes)))


(defmethod print-object ((obj params) out)
    (format out "{text-document: ~A; content-changes: ~A}"
            (text-document obj)
            (content-changes obj)))


(defmethod types:deep-equal-p ((a params) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (text-document a) (text-document b))
         (types:deep-equal-p (content-changes a) (content-changes b))))


(defclass content-change ()
    ((text :accessor text
           :initform nil
           :initarg :text)))


(defmethod print-object ((obj content-change) out)
    (format out "{text: \"~A\"}" (text obj)))


(defmethod types:deep-equal-p ((a content-change) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (text a) (text b))))


(defun get-uri (msg)
    (let* ((params (message:params msg))
           (doc (text-document params)))
        (text-doc:uri doc)))


(defun get-text (msg)
    (let* ((params (message:params msg))
           (changes (content-changes params)))

        (when (first changes)
              (text (first changes)))))


(defun change-from-wire (change)
    (labels ((add-param (params key value)
                  (cond ((eq key :text) (setf (text params) value)))))

        (loop :with out-params := (make-instance 'content-change)

              :for param :in change :do
                  (add-param out-params (car param) (cdr param))

              :finally (return out-params))))


(defun changes-from-wire (changes)
    (loop :for change :in changes
          :collect (change-from-wire change)))


(defun from-wire (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :text-document) (setf (text-document params)
                                                       (text-doc:from-wire value)))
                        ((eq key :content-changes) (setf (content-changes params)
                                                         (changes-from-wire value))))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (make-instance 'did-change :params out-params)))))