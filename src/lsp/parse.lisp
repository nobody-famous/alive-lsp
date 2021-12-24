(defpackage :alive/lsp/parse
    (:use :cl)
    (:export :from-stream)

    (:local-nicknames (:init :alive/lsp/message/initialize)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)))

(in-package :alive/lsp/parse)


(defstruct fields
    id
    jsonrpc
    method-name
    params
    result
    error-msg)


(defun trim-ws (str)
    (string-trim (list #\space #\newline #\linefeed #\return)
                 str))


(defun next-line (input)
    (loop :with str := (make-string-output-stream)
          :with prev-char := (code-char 0)
          :until (and (char= #\return prev-char)
                      (char= #\newline
                             (peek-char nil input nil nil)))
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
                  (let ((ch (read-char input nil nil)))
                      (when ch (write-char ch out))))))


(defun decode-json (content)
    (with-input-from-string (str content)
        (json:decode-json str)))


(defun get-msg-fields (payload)
    (loop :with fields := (make-fields)
          :for item :in payload :do
              (cond ((eq :jsonrpc (car item)) (setf (fields-jsonrpc fields) (cdr item)))
                    ((eq :id (car item)) (setf (fields-id fields) (cdr item)))
                    ((eq :method (car item)) (setf (fields-method-name fields) (string-downcase (cdr item))))
                    ((eq :result (car item)) (setf (fields-result fields) (cdr item)))
                    ((eq :error (car item)) (setf (fields-error-msg fields) (cdr item)))
                    ((eq :params (car item)) (setf (fields-params fields) (cdr item))))
          :finally (return fields)))


(defun request-p (fields)
    (fields-method-name fields))


(defun response-p (fields)
    (or (fields-result fields)
        (fields-error-msg fields)))


(defun build-init-req (fields)
    (init:request-from-wire :jsonrpc (fields-jsonrpc fields)
                            :id (fields-id fields)
                            :params (fields-params fields)))


#+n (defun build-initialized (fields)
        (make-instance 'message:request-payload
                       :jsonrpc (fields-jsonrpc fields)
                       :method (fields-method-name fields)
                       :params nil))


(defun build-request (fields)
    (cond ((string= "initialize" (fields-method-name fields)) (build-init-req fields))
          ((string= "initialized" (fields-method-name fields)) (init:create-initialized-notification))
          (T (error (format nil "Unhandled request ~A" fields)))))


(defun build-message (payload)
    (let ((fields (get-msg-fields payload)))
        (cond ((request-p fields) (build-request fields))
              ((response-p fields) (format T "GOT RESPONSE~%"))
              (T (error (format nil "Unknown payload type ~A" payload))))))


(defun from-stream (input)
    (let* ((header (parse-header input))
           (raw-content (read-content input (packet:content-length header)))
           (content (decode-json raw-content)))
        (build-message content)))
