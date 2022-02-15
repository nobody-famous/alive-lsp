(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:did-change :alive/lsp/message/document/did-change)
                      (:did-open :alive/lsp/message/document/did-open)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:text-doc-item :alive/lsp/types/text-doc-item)
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)
                      (:init :alive/lsp/message/initialize)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)

                      (:run :alive/test/harness/run)
                      (:check :alive/test/harness/check)))

(in-package :alive/test/lsp/message)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))


(defun create-msg (content)
    (with-output-to-string (str)
        (format str "Content-Length: ~A~A" (length content) *end-line*)
        (format str "~A" *end-line*)
        (format str "~A" content)))


(defun init-msg ()
    (labels ((create-content ()
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

        (run:test "Initialize Message"
                  (lambda ()
                      (let* ((msg (create-msg (create-content)))
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
                                                              :legend (alive/lsp/message/initialize:create-legend
                                                                       :types (list "comment"
                                                                                    "string"
                                                                                    "keyword"
                                                                                    "number"
                                                                                    "namespace"
                                                                                    "function"
                                                                                    "macro"
                                                                                    "variable"
                                                                                    "parameter"
                                                                                    "parenthesis"
                                                                                    "symbol"))
                                                              :full t))
                                       (alive/lsp/message/initialize::capabilities result))))))


(defun did-open-msg ()
    (labels ((create-content ()
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
                      (format str "}~A" *end-line*))))

        (run:test "Did Open Message"
                  (lambda ()
                      (let* ((msg (create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal (did-open:create-did-open
                                            (did-open:create-params
                                             (text-doc-item:create-item :text "(foo)"
                                                                        :language-id "lisp"
                                                                        :version "1"
                                                                        :uri "file:///some/file.txt")))
                                           parsed))))))


(defun did-change-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" *end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" *end-line*)
                      (format str "  \"id\": 0,~A" *end-line*)
                      (format str "  \"method\": \"textDocument/didChange\",~A" *end-line*)
                      (format str "  \"params\": {~A" *end-line*)
                      (format str "    \"textDocument\": {~A" *end-line*)
                      (format str "      \"uri\": \"file:///some/file.txt\"~A" *end-line*)
                      (format str "    },~A" *end-line*)
                      (format str "    \"contentChanges\": [~A" *end-line*)
                      (format str "      {~A" *end-line*)
                      (format str "        \"text\": \"(foo)\"~A" *end-line*)
                      (format str "      }~A" *end-line*)
                      (format str "    ]~A" *end-line*)
                      (format str "  }~A" *end-line*)
                      (format str "}~A" *end-line*))))

        (run:test "Did Change Message"
                  (lambda ()
                      (let* ((msg (create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (did-change:create
                            (did-change:create-params
                             :text-doc (text-doc:create :uri "file:///some/file.txt")
                             :changes (list (did-change:create-change "(foo)"))))
                           parsed))))))


(defun sem-tokens-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" *end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" *end-line*)
                      (format str "  \"id\": 0,~A" *end-line*)
                      (format str "  \"method\": \"textDocument/semanticTokens/full\",~A" *end-line*)
                      (format str "  \"params\": {~A" *end-line*)
                      (format str "    \"textDocument\": {~A" *end-line*)
                      (format str "      \"uri\": \"file:///some/file.txt\"~A" *end-line*)
                      (format str "    }~A" *end-line*)
                      (format str "  }~A" *end-line*)
                      (format str "}~A" *end-line*))))

        (run:test "Semantic Tokens Message"
                  (lambda ()
                      (let* ((msg (create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (sem-tokens:create-request
                            (sem-tokens:create-params
                             (text-doc:create :uri "file:///some/file.txt")))
                           parsed))))))


(defun run-all ()
    (run:suite "LSP Messages"
               (lambda ()
                   (init-msg)
                   (init-resp-msg)
                   (did-open-msg)
                   (did-change-msg)
                   (sem-tokens-msg))))
