(defpackage :alive/lsp/parse
    (:use :cl)
    (:export :from-stream)

    (:local-nicknames (:completion :alive/lsp/message/document/completion)
                      (:did-open :alive/lsp/message/document/did-open)
                      (:did-change :alive/lsp/message/document/did-change)
                      (:load-file :alive/lsp/message/alive/load-file)
                      (:try-compile :alive/lsp/message/alive/try-compile)
                      (:init :alive/lsp/message/initialize)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:errors :alive/lsp/errors)
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)))

(in-package :alive/lsp/parse)


(defclass fields ()
    ((id :accessor id
         :initform nil
         :initarg :id)
     (jsonrpc :accessor jsonrpc
              :initform nil
              :initarg :jsonrpc)
     (method :accessor method-name
             :initform nil
             :initarg :method)
     (params :accessor params
             :initform nil
             :initarg :params)
     (result :accessor result
             :initform nil
             :initarg :result)
     (error :accessor error-msg
            :initform nil
            :initarg :error)))


(defun trim-ws (str)
    (string-trim (list #\space #\newline #\linefeed #\return)
                 str))


(defun next-line (input)
    (loop :with str := (make-string-output-stream)
          :with prev-char := (code-char 0)
          :until (and (char= #\return prev-char)
                      (char= #\newline
                             (peek-char nil input)))
          :do (let ((ch (read-char input)))
                  (setf prev-char ch)
                  (write-char ch str))
          :finally (progn (read-char input)
                          (return (trim-ws (get-output-stream-string str))))))


(defun get-header-lines (input)
    (loop :with done := nil
          :with lines := ()
          :until done
          :do (let ((line (next-line input)))
                  (if (zerop (length line))
                      (setf done T)
                      (setf lines (cons line lines))))
          :finally (progn
                    (return lines))))


(defun parse-header-line (line)
    (let ((ndx (position #\: line)))
        (unless ndx (error (format nil "Invalid header line: ~A" line)))

        (list (trim-ws (subseq line 0 ndx))
              (trim-ws (subseq line (+ 1 ndx))))))


(defun add-to-header (header pair)
    (destructuring-bind (key value)
            pair
        (cond ((string= key "Content-Length") (setf (packet:content-length header)
                                                    (parse-integer value)))
              (T (error (format nil "Unhandled header key: ~A" key))))))


(defun parse-header (input)
    (loop :with header := (packet:create-header)
          :for line :in (get-header-lines input) :do
              (add-to-header header
                             (parse-header-line line))
          :finally (return header)))


(defun read-content (input size)
    (with-output-to-string (out)
        (loop :for ndx :from 0 :below size :do
                  (let ((ch (read-char input)))
                      (when ch (write-char ch out))))))


(defun decode-json (content)
    (with-input-from-string (str content)
        (json:decode-json str)))


(defun get-msg-fields (payload)
    (loop :with fields := (make-instance 'fields)
          :for item :in payload :do
              (cond ((eq :jsonrpc (car item)) (setf (jsonrpc fields) (cdr item)))
                    ((eq :id (car item)) (setf (id fields) (cdr item)))
                    ((eq :method (car item)) (setf (method-name fields) (string-downcase (cdr item))))
                    ((eq :result (car item)) (setf (result fields) (cdr item)))
                    ((eq :error (car item)) (setf (error-msg fields) (cdr item)))
                    ((eq :params (car item)) (setf (params fields) (cdr item))))
          :finally (return fields)))


(defun request-p (fields)
    (method-name fields))


(defun response-p (fields)
    (or (result fields)
        (error-msg fields)))


(defun build-request (fields)
    (let ((name (string-downcase (method-name fields)))
          (msg-id (id fields)))
        (cond ((string= "initialize" name)
               (init:request-from-wire :jsonrpc (jsonrpc fields)
                                       :id msg-id
                                       :params (params fields)))

              ((string= "initialized" name)
               (init:create-initialized-notification))

              ((string= "textdocument/didopen" name)
               (did-open:from-wire (params fields)))

              ((string= "textdocument/didchange" name)
               (did-change:from-wire (params fields)))

              ((string= "textdocument/completion" name)
               (completion:from-wire :jsonrpc (jsonrpc fields)
                                     :id msg-id
                                     :params (params fields)))

              ((string= "textdocument/semantictokens/full" name)
               (sem-tokens:from-wire :jsonrpc (jsonrpc fields)
                                     :id msg-id
                                     :params (params fields)))

              ((string= "$/alive/loadfile" name)
               (load-file:from-wire :jsonrpc (jsonrpc fields)
                                    :id msg-id
                                    :params (params fields)))

              ((string= "textdocument/didsave" name) nil)

              ((string= "$/alive/trycompile" name)
               (try-compile:from-wire :jsonrpc (jsonrpc fields)
                                      :id msg-id
                                      :params (params fields)))

              (T (error (make-condition 'errors:unhandled-request
                                        :id msg-id
                                        :method-name name))))))

(defun build-message (payload)
    (let ((fields (get-msg-fields payload)))
        (cond ((request-p fields) (build-request fields))
              ((response-p fields) (error (make-condition 'errors:server-error
                                                          :id (id fields)
                                                          :message "Got response")))
              (T (error (make-condition 'errors:server-error
                                        :id (id fields)
                                        :message "Unknown payload type"))))))


(defun from-stream (input)
    (let* ((header (parse-header input))
           (raw-content (read-content input (packet:content-length header)))
           (content (decode-json raw-content)))
        (build-message content)))
