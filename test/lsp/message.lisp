(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:completion :alive/lsp/message/document/completion)
                      (:config :alive/lsp/message/workspace/config)
                      (:did-change :alive/lsp/message/document/did-change)
                      (:did-open :alive/lsp/message/document/did-open)
                      (:hover :alive/lsp/message/document/hover)
                      (:eval :alive/lsp/message/alive/do-eval)
                      (:get-pkg :alive/lsp/message/alive/get-pkg)
                      (:remove-pkg :alive/lsp/message/alive/remove-pkg)
                      (:list-asdf :alive/lsp/message/alive/list-asdf)
                      (:load-asdf :alive/lsp/message/alive/load-asdf)
                      (:list-pkgs :alive/lsp/message/alive/list-packages)
                      (:list-threads :alive/lsp/message/alive/list-threads)
                      (:kill-thread :alive/lsp/message/alive/kill-thread)
                      (:load-file :alive/lsp/message/alive/load-file)
                      (:top-form :alive/lsp/message/alive/top-form)
                      (:unexport :alive/lsp/message/alive/unexport-symbol)
                      (:user-input :alive/lsp/message/alive/user-input)
                      (:config-item :alive/lsp/types/config-item)
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

                      (:utils :alive/test/utils)))

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

        (clue:test "Initialize Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (alive/lsp/message/initialize:create-request
                                                :id 5
                                                :params (alive/lsp/message/initialize:create-request-params
                                                            :client-info (alive/lsp/message/initialize:create-client-info
                                                                             :name "Visual Studio Code"
                                                                             :version "1.62.3")))
                                  :actual parsed)))))


(defun init-resp-msg ()
    (clue:test "Initialize Response"
        (let* ((msg (init:create-response 0))
               (result (alive/lsp/message/abstract:result msg)))
            (clue:check-equal :expected (alive/lsp/message/initialize:create-capabilities
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
                              :actual (alive/lsp/message/initialize::capabilities result)))))


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

        (clue:test "Did Open Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (did-open:create-did-open
                                                (did-open:create-params
                                                    (text-doc-item:create-item :text "(foo)"
                                                                               :language-id "lisp"
                                                                               :version "1"
                                                                               :uri "file:///some/file.txt")))
                                  :actual parsed)))))


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

        (clue:test "Did Change Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (did-change:create
                                                (did-change:create-params
                                                    :text-doc (text-doc:create :uri "file:///some/file.txt")
                                                    :changes (list (did-change:create-change "(foo)"))))
                                  :actual parsed)))))


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

        (clue:test "Semantic Tokens Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (sem-tokens:create-request
                                                :id 5
                                                :params (sem-tokens:create-params
                                                            (text-doc:create :uri "file:///some/file.txt")))
                                  :actual parsed)))))


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

        (clue:test "Load File Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (load-file:create-request
                                                :id 5
                                                :params (load-file:create-params :path "file:///some/file.txt"
                                                                                 :show-stdout nil))
                                  :actual parsed)))))


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

        (clue:test "Completion Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (completion:create-request
                                                :id 5
                                                :params (completion:create-params :text-document (text-doc:create :uri "file:///some/file.txt")
                                                                                  :pos (pos:create 3 11)))
                                  :actual parsed)))))


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

        (clue:test "Top Form Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (top-form:create-request
                                                :id 5
                                                :params (top-form:create-params :text-document (text-doc:create :uri "file:///some/file.txt")
                                                                                :pos (pos:create 5 10)))
                                  :actual parsed)))))


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

        (clue:test "Formatting Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (formatting:create-request
                                                :id 5
                                                :params (formatting:create-params :range (range:create (pos:create 0 0) (pos:create 10 10))
                                                                                  :text-document (text-doc:create :uri "file:///some/file.txt")))
                                  :actual parsed)))))


(defun list-threads-msg ()
    (labels ((create-content ()
                             (with-output-to-string (str)
                                 (format str "{~A" utils:*end-line*)
                                 (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                                 (format str "  \"id\": 5,~A" utils:*end-line*)
                                 (format str "  \"method\": \"$/alive/listThreads\"~A" utils:*end-line*)
                                 (format str "}~A" utils:*end-line*))))

        (clue:test "List Threads Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (list-threads:create-request
                                                :id 5)
                                  :actual parsed)))))


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

        (clue:test "Kill Thread Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (kill-thread:create-request
                                                :id 5
                                                :params (kill-thread:create-params :id 10))
                                  :actual parsed)))))


(defun list-pkgs-msg ()
    (labels ((create-content ()
                             (with-output-to-string (str)
                                 (format str "{~A" utils:*end-line*)
                                 (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                                 (format str "  \"id\": 5,~A" utils:*end-line*)
                                 (format str "  \"method\": \"$/alive/listPackages\"~A" utils:*end-line*)
                                 (format str "}~A" utils:*end-line*))))

        (clue:test "List Packages Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (list-pkgs:create-request
                                                :id 5)
                                  :actual parsed)))))


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

        (clue:test "Unexport Symbol Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (unexport:create-request
                                                :id 5
                                                :params (unexport:create-params :sym-name "foo" :pkg-name "bar"))
                                  :actual parsed)))))


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

        (clue:test "Eval Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (eval:create-request
                                                :id 5
                                                :params (eval:create-params :pkg-name "foo" :text "(+ 1 2)"))
                                  :actual parsed)))))


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

        (clue:test "Get Package Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (get-pkg:create-request
                                                :id 5
                                                :params (get-pkg:create-params :text-document (text-doc:create :uri "file:///some/file.txt")
                                                                               :pos (pos:create 5 10)))
                                  :actual parsed)))))


(defun remove-pkg-msg ()
    (labels ((create-content ()
                             (with-output-to-string (str)
                                 (format str "{~A" utils:*end-line*)
                                 (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                                 (format str "  \"id\": 5,~A" utils:*end-line*)
                                 (format str "  \"method\": \"$/alive/removePackage\",~A" utils:*end-line*)
                                 (format str "  \"params\": {~A" utils:*end-line*)
                                 (format str "    \"name\": \"foo\"~A" utils:*end-line*)
                                 (format str "  }~A" utils:*end-line*)
                                 (format str "}~A" utils:*end-line*))))

        (clue:test "Remove Package Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (remove-pkg:create-request
                                                :id 5
                                                :params (remove-pkg:create-params :name "foo"))
                                  :actual parsed)))))


(defun list-asdf-msg ()
    (labels ((create-content ()
                             (with-output-to-string (str)
                                 (format str "{~A" utils:*end-line*)
                                 (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                                 (format str "  \"id\": 5,~A" utils:*end-line*)
                                 (format str "  \"method\": \"$/alive/listAsdfSystems\"~A" utils:*end-line*)
                                 (format str "}~A" utils:*end-line*))))

        (clue:test "List ASDF Systems Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (list-asdf:create-request
                                                :id 5)
                                  :actual parsed)))))


(defun load-asdf-system-msg ()
    (labels ((create-content ()
                             (with-output-to-string (str)
                                 (format str "{~A" utils:*end-line*)
                                 (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                                 (format str "  \"id\": 5,~A" utils:*end-line*)
                                 (format str "  \"method\": \"$/alive/loadAsdfSystem\",~A" utils:*end-line*)
                                 (format str "  \"params\": {~A" utils:*end-line*)
                                 (format str "    \"name\": \"foo\"~A" utils:*end-line*)
                                 (format str "  }~A" utils:*end-line*)
                                 (format str "}~A" utils:*end-line*))))

        (clue:test "Load ASDF System Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (load-asdf:create-request
                                                :id 5
                                                :params (load-asdf:create-params :name "foo"))
                                  :actual parsed)))))


(defun hover-msg ()
    (labels ((create-content ()
                             (with-output-to-string (str)
                                 (format str "{~A" utils:*end-line*)
                                 (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                                 (format str "  \"id\": 5,~A" utils:*end-line*)
                                 (format str "  \"method\": \"textdocument/hover\",~A" utils:*end-line*)
                                 (format str "  \"params\": {~A" utils:*end-line*)
                                 (format str "    \"textDocument\": {~A" utils:*end-line*)
                                 (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                                 (format str "    },~A" utils:*end-line*)
                                 (format str "    \"position\": {~A" utils:*end-line*)
                                 (format str "      \"line\": 3,~A" utils:*end-line*)
                                 (format str "      \"character\": 11~A" utils:*end-line*)
                                 (format str "    }~A" utils:*end-line*)
                                 (format str "  }~A" utils:*end-line*)
                                 (format str "}~A" utils:*end-line*))))

        (clue:test "Hover Message"
            (let* ((msg (utils:create-msg (create-content)))
                   (parsed (parse:from-stream (utils:stream-from-string msg))))
                (clue:check-equal :expected (hover:create-request
                                                :id 5
                                                :params (hover:create-params :text-document (text-doc:create :uri "file:///some/file.txt")
                                                                             :pos (pos:create 3 11)))
                                  :actual parsed)))))


(defun run-all ()
    (clue:suite "LSP Messages"
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
        (get-pkg-msg)
        (list-asdf-msg)
        (load-asdf-system-msg)
        (hover-msg)))