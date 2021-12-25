(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:did-open :alive/lsp/message/document/did-open)
                      (:init :alive/lsp/message/initialize)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)))

(in-package :alive/test/lsp/message)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))


(defun create-init-content ()
    (with-output-to-string (str)
        (format str "{~A" *end-line*)
        (format str "  \"jsonrpc\": \"2.0\",~A" *end-line*)
        (format str "  \"id\": 0,~A" *end-line*)
        (format str "  \"method\": \"initialize\",~A" *end-line*)
        (format str "  \"params\": {~A" *end-line*)
        (format str "    \"clientInfo\": {~A" *end-line*)
        (format str "      \"name\": \"Visual Studio Code\",~A" *end-line*)
        (format str "      \"version\": \"1.62.3\"~A" *end-line*)
        (format str "    }~A" *end-line*)
        (format str "  }~A" *end-line*)
        (format str "}~A" *end-line*)))


(defun create-did-open-content ()
    (with-output-to-string (str)
        (format str "{~A" *end-line*)
        (format str "  \"jsonrpc\": \"2.0\",~A" *end-line*)
        (format str "  \"id\": 0,~A" *end-line*)
        (format str "  \"method\": \"textDocument/didOpen\",~A" *end-line*)
        (format str "  \"params\": {~A" *end-line*)
        (format str "    \"textDocument\": {~A" *end-line*)
        (format str "      \"uri\": \"file:///some/file.txt\",~A" *end-line*)
        (format str "      \"languageId\": \"lisp\",~A" *end-line*)
        (format str "      \"version\": \"1\",~A" *end-line*)
        (format str "      \"text\": \"(foo)\"~A" *end-line*)
        (format str "    }~A" *end-line*)
        (format str "  }~A" *end-line*)
        (format str "}~A" *end-line*)))


(defun create-msg (content)
    (with-output-to-string (str)
        (format str "Content-Length: ~A~A" (length content) *end-line*)
        (format str "~A" *end-line*)
        (format str "~A" content)))


(defun parse-msg ()
    (let* ((msg (create-msg (create-init-content)))
           (parsed (parse:from-stream (make-string-input-stream msg))))
        (format T "PARSED ~A~%" parsed)
        (format T "JSON ~A~%" (json:encode-json-to-string parsed))))


(defun resp-msg ()
    (let ((msg (init:create-response 0)))
        (format T "resp-msg ~A~%" (parse:from-stream (make-string-input-stream (packet:to-wire msg))))
        (format T "resp-msg ~A~%" (json:encode-json-to-string msg))))


(defun did-open-msg ()
    (let* ((msg (create-msg (create-did-open-content)))
           (parsed (parse:from-stream (make-string-input-stream msg))))
        (format T "did-open text ~A~%" (did-open:get-text parsed))
        (format T "did-open uri ~A~%" (did-open:get-uri parsed))))


(defun run-all ()
    (parse-msg)
    (resp-msg)
    (did-open-msg))
