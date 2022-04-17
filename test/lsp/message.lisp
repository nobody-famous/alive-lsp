(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:completion :alive/lsp/message/document/completion)
                      (:did-change :alive/lsp/message/document/did-change)
                      (:did-open :alive/lsp/message/document/did-open)
                      (:eval :alive/lsp/message/alive/do-eval)
                      (:get-pkg :alive/lsp/message/alive/get-pkg)
                      (:list-pkgs :alive/lsp/message/alive/list-packages)
                      (:list-threads :alive/lsp/message/alive/list-threads)
                      (:kill-thread :alive/lsp/message/alive/kill-thread)
                      (:load-file :alive/lsp/message/alive/load-file)
                      (:top-form :alive/lsp/message/alive/top-form)
                      (:unexport :alive/lsp/message/alive/unexport-symbol)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:text-doc-item :alive/lsp/types/text-doc-item)
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)
                      (:formatting :alive/lsp/message/document/range-format)
                      (:init :alive/lsp/message/initialize)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:message :alive/lsp/message/abstract)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)

                      (:utils :alive/test/utils)
                      (:run :alive/test/harness/run)
                      (:check :alive/test/harness/check)))

(in-package :alive/test/lsp/message)


(defun init-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"initialize\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"clientInfo\": {~A" utils:*end-line*)
                      (format str "      \"name\": \"Visual Studio Code\",~A" utils:*end-line*)
                      (format str "      \"version\": \"1.62.3\"~A" utils:*end-line*)
                      (format str "    }~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Initialize Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal (alive/lsp/message/initialize:create-request
                                            :id 5
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
                                        :hover-provider nil
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
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"textDocument/didOpen\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"textDocument\": {~A" utils:*end-line*)
                      (format str "      \"uri\": \"file:///some/file.txt\",~A" utils:*end-line*)
                      (format str "      \"languageId\": \"lisp\",~A" utils:*end-line*)
                      (format str "      \"version\": \"1\",~A" utils:*end-line*)
                      (format str "      \"text\": \"(foo)\"~A" utils:*end-line*)
                      (format str "    }~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Did Open Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
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
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"textDocument/didChange\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"textDocument\": {~A" utils:*end-line*)
                      (format str "      \"uri\": \"file:///some/file.txt\"~A" utils:*end-line*)
                      (format str "    },~A" utils:*end-line*)
                      (format str "    \"contentChanges\": [~A" utils:*end-line*)
                      (format str "      {~A" utils:*end-line*)
                      (format str "        \"text\": \"(foo)\"~A" utils:*end-line*)
                      (format str "      }~A" utils:*end-line*)
                      (format str "    ]~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Did Change Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
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
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"textDocument/semanticTokens/full\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"textDocument\": {~A" utils:*end-line*)
                      (format str "      \"uri\": \"file:///some/file.txt\"~A" utils:*end-line*)
                      (format str "    }~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Semantic Tokens Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (sem-tokens:create-request
                            :id 5
                            :params (sem-tokens:create-params
                                     (text-doc:create :uri "file:///some/file.txt")))
                           parsed))))))


(defun load-file-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/loadFile\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"path\": \"file:///some/file.txt\",~A" utils:*end-line*)
                      (format str "    \"showStdout\": false~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Load File Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (load-file:create-request
                            :id 5
                            :params (load-file:create-params :path "file:///some/file.txt"
                                                             :show-stdout nil))
                           parsed))))))


(defun completion-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"textdocument/completion\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"textDocument\": {~A" utils:*end-line*)
                      (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                      (format str "    },~A" utils:*end-line*)
                      (format str "    \"position\": {~A" utils:*end-line*)
                      (format str "      \"line\": 3,~A" utils:*end-line*)
                      (format str "      \"character\": 11~A" utils:*end-line*)
                      (format str "    },~A" utils:*end-line*)
                      (format str "    \"context\": {~A" utils:*end-line*)
                      (format str "      \"triggerKind\": 1~A" utils:*end-line*)
                      (format str "    }~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Completion Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (completion:create-request
                            :id 5
                            :params (completion:create-params :text-document (text-doc:create :uri "file:///some/file.txt")
                                                              :pos (pos:create 3 11)))
                           parsed))))))


(defun top-form-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/topFormBounds\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"textDocument\": {~A" utils:*end-line*)
                      (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                      (format str "    },~A" utils:*end-line*)
                      (format str "    \"position\": {~A" utils:*end-line*)
                      (format str "      \"line\": 5,~A" utils:*end-line*)
                      (format str "      \"character\": 10~A" utils:*end-line*)
                      (format str "    }~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Top Form Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (top-form:create-request
                            :id 5
                            :params (top-form:create-params :text-document (text-doc:create :uri "file:///some/file.txt")
                                                            :pos (pos:create 5 10)))
                           parsed))))))


(defun formatting-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"textdocument/rangeformatting\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"textDocument\": {~A" utils:*end-line*)
                      (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                      (format str "    },~A" utils:*end-line*)
                      (format str "    \"range\": {~A" utils:*end-line*)
                      (format str "      \"start\": {~A" utils:*end-line*)
                      (format str "        \"line\": 0,~A" utils:*end-line*)
                      (format str "        \"character\": 0~A" utils:*end-line*)
                      (format str "      },~A" utils:*end-line*)
                      (format str "      \"end\": {~A" utils:*end-line*)
                      (format str "        \"line\": 10,~A" utils:*end-line*)
                      (format str "        \"character\": 10~A" utils:*end-line*)
                      (format str "      }~A" utils:*end-line*)
                      (format str "    },~A" utils:*end-line*)
                      (format str "    \"options\": {~A" utils:*end-line*)
                      (format str "      \"tabSize\": 4,~A" utils:*end-line*)
                      (format str "      \"insertSpaces\": true~A" utils:*end-line*)
                      (format str "    }~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Formatting Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (formatting:create-request
                            :id 5
                            :params (formatting:create-params :range (range:create (pos:create 0 0) (pos:create 10 10))
                                                              :text-document (text-doc:create :uri "file:///some/file.txt")))
                           parsed))))))


(defun list-threads-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/listThreads\"~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "List Threads Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (list-threads:create-request
                            :id 5)
                           parsed))))))


(defun kill-thread-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/killThread\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"id\": 10~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Kill Thread Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (kill-thread:create-request
                            :id 5
                            :params (kill-thread:create-params :id 10))
                           parsed))))))


(defun list-pkgs-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/listPackages\"~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "List Packages Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (list-pkgs:create-request
                            :id 5)
                           parsed))))))


(defun unexport-symbol-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/unexportSymbol\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"symbol\": \"foo\",~A" utils:*end-line*)
                      (format str "    \"package\": \"bar\"~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Unexport Symbol Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (unexport:create-request
                            :id 5
                            :params (unexport:create-params :sym-name "foo" :pkg-name "bar"))
                           parsed))))))


(defun eval-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/eval\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"package\": \"foo\",~A" utils:*end-line*)
                      (format str "    \"text\": \"(+ 1 2)\"~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Eval Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (eval:create-request
                            :id 5
                            :params (eval:create-params :pkg-name "foo" :text "(+ 1 2)"))
                           parsed))))))


(defun get-pkg-msg ()
    (labels ((create-content ()
                  (with-output-to-string (str)
                      (format str "{~A" utils:*end-line*)
                      (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                      (format str "  \"id\": 5,~A" utils:*end-line*)
                      (format str "  \"method\": \"$/alive/getPackageForPosition\",~A" utils:*end-line*)
                      (format str "  \"params\": {~A" utils:*end-line*)
                      (format str "    \"textDocument\": {~A" utils:*end-line*)
                      (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                      (format str "    },~A" utils:*end-line*)
                      (format str "    \"position\": {~A" utils:*end-line*)
                      (format str "      \"line\": 5,~A" utils:*end-line*)
                      (format str "      \"character\": 10~A" utils:*end-line*)
                      (format str "    }~A" utils:*end-line*)
                      (format str "  }~A" utils:*end-line*)
                      (format str "}~A" utils:*end-line*))))

        (run:test "Get Package Message"
                  (lambda ()
                      (let* ((msg (utils:create-msg (create-content)))
                             (parsed (parse:from-stream (make-string-input-stream msg))))
                          (check:are-equal
                           (get-pkg:create-request
                            :id 5
                            :params (get-pkg:create-params :text-document (text-doc:create :uri "file:///some/file.txt")
                                                           :pos (pos:create 5 10)))
                           parsed))))))


(defun run-all ()
    (run:suite "LSP Messages"
               (lambda ()
                   (init-msg)
                   (init-resp-msg)
                   (did-open-msg)
                   (did-change-msg)
                   (sem-tokens-msg)
                   (load-file-msg)
                   (completion-msg)
                   (top-form-msg)
                   (formatting-msg)
                   (list-threads-msg)
                   (kill-thread-msg)
                   (list-pkgs-msg)
                   (unexport-symbol-msg)
                   (eval-msg)
                   (get-pkg-msg))))
