(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:parse :alive/lsp/parse)
                      (:types :alive/lsp/types)
                      (:init-req :alive/lsp/init-request)
                      (:init-res :alive/lsp/init-response)
    ))

(in-package :alive/test/lsp/message)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))


(defun create-content ()
    (with-output-to-string (str)
        (format str "{~A" *end-line*)
        (format str "  \"jsonrpc\": 2.0,~A" *end-line*)
        (format str "  \"id\": 0,~A" *end-line*)
        (format str "  \"method\": \"initialize\",~A" *end-line*)
        (format str "  \"params\": {~A" *end-line*)
        (format str "    \"client-info\": {~A" *end-line*)
        (format str "      \"name\": \"Visual Studio Code\",~A" *end-line*)
        (format str "      \"version\": \"1.62.3\"~A" *end-line*)
        (format str "    }~A" *end-line*)
        (format str "  }~A" *end-line*)
        (format str "}~A" *end-line*)
    ))


(defun create-msg ()
    (with-output-to-string (str)
        (let ((content (create-content)))
            (format str "Content-Length: ~A~A" (length content) *end-line*)
            (format str "~A" *end-line*)
            (format str "~A" content)
        )))


(defun parse-msg ()
    (let* ((msg (create-msg))
           (parsed (parse:from-stream (make-string-input-stream msg)))
          )
        (format T "PARSED ~A~%" parsed)
        (format T "JSON ~A~%" (json:encode-json parsed))
    ))


(defun resp-msg ()
    (let ((payload (make-instance 'types:response-payload
                                  :id 0
                                  :result (make-instance 'init-res:result)
                   )))
        (format T "JSON ~A~%" (json:encode-json payload))
    ))


(defun run-all ()
    (parse-msg)
    (resp-msg)
)
