(defpackage :alive/lsp/parse
    (:use :cl)
    (:export :from-stream)

    (:local-nicknames (:completion :alive/lsp/message/document/completion)
                      (:did-open :alive/lsp/message/document/did-open)
                      (:did-change :alive/lsp/message/document/did-change)
                      (:hover :alive/lsp/message/document/hover)
                      (:formatting :alive/lsp/message/document/range-format)
                      (:on-type :alive/lsp/message/document/fmt-on-type)
                      (:eval :alive/lsp/message/alive/do-eval)
                      (:inspect :alive/lsp/message/alive/do-inspect)
                      (:get-pkg :alive/lsp/message/alive/get-pkg)
                      (:remove-pkg :alive/lsp/message/alive/remove-pkg)
                      (:list-asdf :alive/lsp/message/alive/list-asdf)
                      (:list-pkgs :alive/lsp/message/alive/list-packages)
                      (:list-threads :alive/lsp/message/alive/list-threads)
                      (:kill-thread :alive/lsp/message/alive/kill-thread)
                      (:load-asdf :alive/lsp/message/alive/load-asdf)
                      (:load-file :alive/lsp/message/alive/load-file)
                      (:top-form :alive/lsp/message/alive/top-form)
                      (:try-compile :alive/lsp/message/alive/try-compile)
                      (:unexport :alive/lsp/message/alive/unexport-symbol)
                      (:init :alive/lsp/message/initialize)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:errors :alive/lsp/errors)
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)))

(in-package :alive/lsp/parse)


(defvar *return-char* (char-code #\return))
(defvar *nl-char* (char-code #\newline))


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
    (loop :with out := (flexi-streams:make-in-memory-output-stream)
          :with prev-char := 0

          :until (and (eq *return-char* prev-char)
                      (eq *nl-char* (flexi-streams:peek-byte input)))

          :do (let ((ch (read-byte input)))
                  (setf prev-char ch)
                  (write-byte ch out))

          :finally (progn (read-byte input)
                          (return (trim-ws (flexi-streams:octets-to-string
                                               (flexi-streams:get-output-stream-sequence out)))))))


(defun get-header-lines (input)
    (loop :with done := nil
          :with lines := ()
          :until done
          :do (let ((line (next-line input)))
                  (if (zerop (length line))
                      (setf done T)
                      (setf lines (cons line lines))))
          :finally (return lines)))


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


(defun read-content-bytes (input size)
    (flexi-streams:with-output-to-sequence (out)
        (loop :for ndx :from 0 :below size :do
                  (let ((ch (read-byte input)))
                      (when ch (write-byte ch out))))))

(defun read-content (input size)
    (let ((content (flexi-streams:octets-to-string
                       (read-content-bytes input size))))
        content))


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

              ((string= "textdocument/hover" name)
                  (hover:from-wire :jsonrpc (jsonrpc fields)
                                   :id msg-id
                                   :params (params fields)))

              ((string= "textdocument/rangeformatting" name)
                  (formatting:from-wire :jsonrpc (jsonrpc fields)
                                        :id msg-id
                                        :params (params fields)))

              ((string= "textdocument/ontypeformatting" name)
                  (on-type:from-wire :jsonrpc (jsonrpc fields)
                                     :id msg-id
                                     :params (params fields)))

              ((string= "textdocument/semantictokens/full" name)
                  (sem-tokens:from-wire :jsonrpc (jsonrpc fields)
                                        :id msg-id
                                        :params (params fields)))

              ((string= "$/alive/eval" name)
                  (eval:from-wire :jsonrpc (jsonrpc fields)
                                  :id msg-id
                                  :params (params fields)))

              ((string= "$/alive/inspect" name)
                  (inspect:from-wire :jsonrpc (jsonrpc fields)
                                     :id msg-id
                                     :params (params fields)))

              ((string= "$/alive/getpackageforposition" name)
                  (get-pkg:from-wire :jsonrpc (jsonrpc fields)
                                     :id msg-id
                                     :params (params fields)))

              ((string= "$/alive/removepackage" name)
                  (remove-pkg:from-wire :jsonrpc (jsonrpc fields)
                                        :id msg-id
                                        :params (params fields)))

              ((string= "$/alive/loadasdfsystem" name)
                  (load-asdf:from-wire :jsonrpc (jsonrpc fields)
                                       :id msg-id
                                       :params (params fields)))

              ((string= "$/alive/loadfile" name)
                  (load-file:from-wire :jsonrpc (jsonrpc fields)
                                       :id msg-id
                                       :params (params fields)))

              ((string= "$/alive/listthreads" name)
                  (list-threads:from-wire :jsonrpc (jsonrpc fields)
                                          :id msg-id
                                          :params (params fields)))

              ((string= "$/alive/killthread" name)
                  (kill-thread:from-wire :jsonrpc (jsonrpc fields)
                                         :id msg-id
                                         :params (params fields)))

              ((string= "$/alive/listasdfsystems" name)
                  (list-asdf:from-wire :jsonrpc (jsonrpc fields)
                                       :id msg-id
                                       :params (params fields)))

              ((string= "$/alive/listpackages" name)
                  (list-pkgs:from-wire :jsonrpc (jsonrpc fields)
                                       :id msg-id
                                       :params (params fields)))

              ((string= "textdocument/didsave" name) nil)

              ((string= "$/alive/trycompile" name)
                  (try-compile:from-wire :jsonrpc (jsonrpc fields)
                                         :id msg-id
                                         :params (params fields)))

              ((string= "$/alive/unexportsymbol" name)
                  (unexport:from-wire :jsonrpc (jsonrpc fields)
                                      :id msg-id
                                      :params (params fields)))

              ((string= "$/alive/topformbounds" name)
                  (top-form:from-wire :jsonrpc (jsonrpc fields)
                                      :id msg-id
                                      :params (params fields)))

              (T (error (make-condition 'errors:unhandled-request
                            :id msg-id
                            :method-name name))))))


(defun build-error-response (fields)
    (message:error-from-wire :id (id fields)
                             :params (error-msg fields)))


(defun build-result-response (fields)
    (message:create-result-resp :id (id fields)
                                :result (result fields)))


(defun build-response (fields)
    (cond ((error-msg fields) (build-error-response fields))
          ((result fields) (build-result-response fields))
          (T (error (make-condition 'errors:server-error
                        :id (id fields)
                        :message "Unknown response type")))))


(defun build-message (payload)
    (let ((fields (get-msg-fields payload)))
        (cond ((request-p fields) (build-request fields))
              ((response-p fields) (build-response fields))
              (T (error (make-condition 'errors:server-error
                            :id (id fields)
                            :message "Unknown payload type"))))))


(defun from-stream (input)
    (let* ((header (parse-header input))
           (raw-content (read-content input (packet:content-length header)))
           (content (decode-json raw-content)))
        (build-message content)))