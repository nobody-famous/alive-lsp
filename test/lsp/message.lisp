(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:did-open :alive/lsp/message/document/did-open)
                      (:init :alive/lsp/message/initialize)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)

                      (:run :alive/test/harness/run)
                      (:check :alive/test/harness/check)))

(in-package :alive/test/lsp/message)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))


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


(defun parse-init-msg ()
    (labels ((create-init-content ()
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
                      (format str "}~A" *end-line*))))

        (run:test "Parse Message"
                  (lambda ()
                      (let* ((msg (create-msg (create-init-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal (alive/lsp/message/initialize:create-request
                                            :id 1
                                            :params (alive/lsp/message/initialize:create-request-params
                                                     :client-info (alive/lsp/message/initialize:create-client-info
                                                                   :name "Visual Studio Code"
                                                                   :version "1.62.3")))
                                           parsed))))))


(defun init-resp-msg ()
    (run:test "Initialize Response"
              (lambda ()
                  (let* ((msg (init:create-response 0))
                         (result (alive/lsp/message/abstract:result msg)))
                      (check:are-equal (alive/lsp/message/initialize:create-capabilities
                                        :text-doc-sync 1
                                        :hover-provider t
                                        :sem-tokens-provider (alive/lsp/message/initialize:create-sem-tokens-opts
                                                              :legend (list "comment"
                                                                            "string"
                                                                            "keyword"
                                                                            "number"
                                                                            "namespace"
                                                                            "function"
                                                                            "macro"
                                                                            "variable"
                                                                            "parameter"
                                                                            "parenthesis"
                                                                            "symbol")
                                                              :full t))
                                       (alive/lsp/message/initialize::capabilities result))
                      (format T "resp-msg ~A~%" (parse:from-stream (make-string-input-stream (packet:to-wire msg))))
                      (format T "resp-msg ~A~%" (json:encode-json-to-string msg))))))


(defun did-open-msg ()
    (let* ((msg (create-msg (create-did-open-content)))
           (parsed (parse:from-stream (make-string-input-stream msg))))
        (format T "did-open text ~A~%" (did-open:get-text parsed))
        (format T "did-open uri ~A~%" (did-open:get-uri parsed))))


(defun run-all ()
    (run:suite "LSP Messages"
               (lambda ()
                   (parse-init-msg)
                   (init-resp-msg)
                   (did-open-msg))))
