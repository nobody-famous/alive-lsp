(defpackage :alive/lsp/message/document/sem-tokens-full
    (:use :cl)
    (:export :create-response
             :req-from-wire
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:text-doc :alive/lsp/types/text-doc)))

(in-package :alive/lsp/message/document/sem-tokens-full)


(defclass request (message:request)
    ((message::method :initform "textDocument/semanticTokens/full")))


(defclass req-params ()
    ((text-document :accessor text-document
                    :initform nil
                    :initarg :text-document)))


(defclass response (message:result-response)
    ())


(defclass sem-tokens ()
    ((data :accessor data
           :initform nil
           :initarg :data)))


(defun get-sem-tokens ()
    (make-instance 'sem-tokens :data (list 0 0 2 0 0)))


(defun get-file-path (uri)
    (merge-pathnames (pathname (purl:url-path (purl:url uri)))
                     #p"/"))


(defun read-tokens (path)
    (with-open-file (f path :if-does-not-exist nil)
        (tokenizer:from-stream f)))


(defun is-keyword (token)
    (and (eq types:*symbol* (token:type-value token))
         (find-symbol (string-upcase (token:text token)) :cl-user)))


(defun create-response (msg)
    (let* ((params (message:params msg))
           (doc (text-document params))
           (path (get-file-path (text-doc:uri doc)))
           (tokens (read-tokens path)))

        (loop :for token :in tokens :do
                  (format T "TOKEN ~A ~A~%"
                          token
                          (if (is-keyword token)
                              "T"
                              "")))

        (format T "SEM TOKENS RESPONSE ~A~%" (json:encode-json-to-string msg))
        (format T "PATH ~A~%" path))

    (make-instance 'response
                   :id (message:id msg)
                   :result (get-sem-tokens)))


(defun req-from-wire (&key jsonrpc id params)
    (labels ((add-param (out-params key value)
                  (cond ((eq key :text-document) (setf (text-document out-params) (text-doc:id-from-wire value))))))

        (loop :with out-params := (make-instance 'req-params)

              :for param :in params :do
                  (add-param out-params (car param) (cdr param))

              :finally (return (make-instance 'request
                                              :jsonrpc jsonrpc
                                              :id id
                                              :params out-params)))))
