(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:init :alive/lsp/message/initialize)))

(in-package :alive/test/lsp/message)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))


(defun create-content ()
    (with-output-to-string (str)
        (format str "{~A" *end-line*)
        (format str "  \"jsonrpc\": \"2.0\",~A" *end-line*)
        (format str "  \"id\": 0,~A" *end-line*)
        (format str "  \"method\": \"initialize\",~A" *end-line*)
        (format str "  \"params\": {~A" *end-line*)
        (format str "    \"client-info\": {~A" *end-line*)
        (format str "      \"name\": \"Visual Studio Code\",~A" *end-line*)
        (format str "      \"version\": \"1.62.3\"~A" *end-line*)
        (format str "    }~A" *end-line*)
        (format str "  }~A" *end-line*)
        (format str "}~A" *end-line*)))


(defun create-msg ()
    (with-output-to-string (str)
        (let ((content (create-content)))
            (format str "Content-Length: ~A~A" (length content) *end-line*)
            (format str "~A" *end-line*)
            (format str "~A" content))))


(defun parse-msg ()
    (let* ((msg (create-msg))
           (parsed (parse:from-stream (make-string-input-stream msg))))
        (format T "PARSED ~A~%" parsed)
        (format T "JSON ~A~%" (json:encode-json-to-string parsed))))


(defun resp-msg ()
    (let ((msg (init:create-response 0)))
        (format T "MSG ~A~%" (message::id msg))
        (format T "JSON ~A~%" (parse:from-stream (make-string-input-stream (packet:to-wire msg))))
        (format T "JSON ~A~%" (json:encode-json-to-string msg))))


(defun run-all ()
    (parse-msg)
    (resp-msg))
