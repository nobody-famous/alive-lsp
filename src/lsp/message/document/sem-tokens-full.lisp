(defpackage :alive/lsp/message/document/sem-tokens-full
    (:use :cl)
    (:export :create-response
             :req-from-wire
             :request)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:analysis :alive/lsp/sem-analysis)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:sem-types :alive/lsp/types/sem-tokens)))

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


(defun create-response (msg)
    (let* ((params (message:params msg))
           (doc (text-document params))
           (path (get-file-path (text-doc:uri doc)))
           (tokens (read-tokens path))
           (sem-tokens (analysis:to-sem-tokens tokens)))

        (make-instance 'response
                       :id (message:id msg)
                       :result (make-instance 'sem-tokens
                                              :data (to-sem-array sem-tokens)))))


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
