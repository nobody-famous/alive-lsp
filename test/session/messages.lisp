(defpackage :alive/test/session/messages
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)
                      (:utils :alive/test/utils)
                      (:sem-tokens :alive/lsp/types/sem-tokens)
                      (:session :alive/session)))

(in-package :alive/test/session/messages)


(defclass test-state (session::state)
        ((send-called :accessor send-called
                      :initform nil
                      :initarg :send-called)))


(defclass inspect-state (test-state)
        ())


(defclass load-asdf-state (test-state)
        ())


(defun create-state (cls)
    (make-instance cls))


(defclass init-msg-state (test-state)
        ())


(defmethod session::get-input-stream ((obj init-msg-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 0,~A" utils:*end-line*)
                       (format str "  \"method\": \"initialize\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"clientInfo\": {~A" utils:*end-line*)
                       (format str "      \"name\": \"Visual Studio Code\",~A" utils:*end-line*)
                       (format str "      \"version\": \"1.62.3\"~A" utils:*end-line*)
                       (format str "    }~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun init-msg ()
    (let ((state (make-instance 'init-msg-state)))
        (clue:test "Initialize Message"
            (let ((sem-tok-legend (make-hash-table :test #'equalp))
                  (sem-tok-opts (make-hash-table :test #'equalp))
                  (on-type-opts (make-hash-table :test #'equalp))
                  (comp-opts (make-hash-table :test #'equalp))
                  (caps (make-hash-table :test #'equalp))
                  (result (make-hash-table :test #'equalp))
                  (exp (make-hash-table :test #'equalp)))
                (setf (gethash "tokenTypes" sem-tok-legend) sem-tokens:*types*)
                (setf (gethash "tokenModifiers" sem-tok-legend) sem-tokens:*mods*)

                (setf (gethash "legend" sem-tok-opts) sem-tok-legend)
                (setf (gethash "full" sem-tok-opts) T)

                (setf (gethash "firstTriggerCharacter" on-type-opts) #\newline)
                (setf (gethash "moreTriggerCharacters" on-type-opts) (list))

                (setf (gethash "triggerCharacters" comp-opts) (list #\:))

                (setf (gethash "textDocumentSync" caps) 1)
                (setf (gethash "hoverProvider" caps) nil)
                (setf (gethash "semanticTokensProvider" caps) sem-tok-opts)
                (setf (gethash "completionProvider" caps) comp-opts)
                (setf (gethash "documentRangeFormattingProvider" caps) T)
                (setf (gethash "documentOnTypeFormattingProvider" caps) on-type-opts)
                (setf (gethash "selectionRangeProvider" caps) T)
                (setf (gethash "definitionProvider" caps) T)
                (setf (gethash "documentSymbolProvider" caps) T)

                (setf (gethash "capabilities" result) caps)

                (setf (gethash "jsonrpc" exp) "2.0")
                (setf (gethash "id" exp) 0)
                (setf (gethash "result" exp) result)

                (utils:check-equal (session::get-next-response state)
                                   exp)))))


(defclass load-file-state (test-state)
        ())


(defmethod session::get-input-stream ((obj load-file-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 0,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/loadFile\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"path\": \"test/files/compile/foo.lisp\",~A" utils:*end-line*)
                       (format str "    \"showStdout\": false~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun load-file-msg ()
    (let ((state (make-instance 'load-file-state)))
        (clue:test "Load File Message"
            (utils:check-equal (session::get-next-response state)
                               (list (cons :jsonrpc "2.0")
                                     (cons :id 0))))))

(defclass completion-state (test-state)
        ())


(defmethod session::get-input-stream ((obj completion-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"textDocument/completion\",~A" utils:*end-line*)
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
        (utils:stream-from-string (utils:create-msg content))))


(defun completion-msg ()
    (let ((state (make-instance 'completion-state)))
        (clue:test "Completion Message"
            (utils:check-has-result (session::get-next-response state)))))


(defclass hover-state (test-state)
        ())


(defmethod session::get-input-stream ((obj hover-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"textDocument/hover\",~A" utils:*end-line*)
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
        (utils:stream-from-string (utils:create-msg content))))


(defun hover-msg ()
    (let ((state (make-instance 'hover-state))
          (exp (make-hash-table :test #'equalp))
          (result (make-hash-table :test #'equalp)))

        (setf (gethash "value" result) "")

        (setf (gethash "jsonrpc" exp) "2.0")
        (setf (gethash "id" exp) 5)
        (setf (gethash "result" exp) result)

        (clue:test "Hover Message"
            (utils:check-equal (session::get-next-response state)
                               exp))))


(defclass top-form-state (test-state)
        ())


(defmethod session::get-input-stream ((obj top-form-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/topFormBounds\",~A" utils:*end-line*)
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
        (utils:stream-from-string (utils:create-msg content))))


(defun top-form-msg ()
    (let ((state (make-instance 'top-form-state))
          (result (make-hash-table :test #'equalp))
          (exp (make-hash-table :test #'equalp)))

        (setf (gethash "start" result) nil)
        (setf (gethash "end" result) nil)

        (setf (gethash "jsonrpc" exp) "2.0")
        (setf (gethash "id" exp) 5)
        (setf (gethash "result" exp) result)

        (clue:test "Top Form Message"
            (utils:check-equal (session::get-next-response state)
                               exp))))


(defclass surrounding-form-state (test-state)
        ())


(defmethod session::get-input-stream ((obj surrounding-form-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/surroundingFormBounds\",~A" utils:*end-line*)
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
        (utils:stream-from-string (utils:create-msg content))))


(defun surrounding-form-msg ()
    (let ((state (make-instance 'surrounding-form-state))
          (result (make-hash-table :test #'equalp))
          (exp (make-hash-table :test #'equalp)))

        (setf (gethash "start" result) nil)
        (setf (gethash "end" result) nil)

        (setf (gethash "jsonrpc" exp) "2.0")
        (setf (gethash "id" exp) 5)
        (setf (gethash "result" exp) result)

        (clue:test "Surrounding Form Message"
            (utils:check-equal (session::get-next-response state)
                               exp))))


(defclass formatting-state (test-state)
        ())


(defmethod session::get-input-stream ((obj formatting-state))
    (let ((content (with-output-to-string (str)
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
        (utils:stream-from-string (utils:create-msg content))))


(defun formatting-msg ()
    (let ((state (make-instance 'formatting-state)))
        (clue:test "Range Format Message"
            (format T "RESP ~A~%" (session::get-next-response state))
            (utils:check-equal (session::get-next-response state)
                               nil))))


(defclass on-type-state (test-state)
        ())


(defmethod session::get-input-stream ((obj on-type-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"textDocument/onTypeFormatting\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"textDocument\": {~A" utils:*end-line*)
                       (format str "      \"uri\":\"file:///some/file.txt\"~A" utils:*end-line*)
                       (format str "    },~A" utils:*end-line*)
                       (format str "    \"position\": {~A" utils:*end-line*)
                       (format str "      \"line\": 3,~A" utils:*end-line*)
                       (format str "      \"character\": 11~A" utils:*end-line*)
                       (format str "    },~A" utils:*end-line*)
                       (format str "    \"ch\": \"\\n\",~A" utils:*end-line*)
                       (format str "    \"options\": {~A" utils:*end-line*)
                       (format str "      \"tabSize\": 4,~A" utils:*end-line*)
                       (format str "      \"insertSpaces\": true~A" utils:*end-line*)
                       (format str "    }~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))

        (utils:stream-from-string (utils:create-msg content))))


(defun format-on-type-msg ()
    (let ((state (make-instance 'on-type-state)))
        (clue:test "Format On Type Message"
            (utils:check-has-result (session::get-next-response state)))))


(defclass list-threads-state (test-state)
        ())


(defmethod session::get-input-stream ((obj list-threads-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/listThreads\"~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun list-threads-msg ()
    (let ((state (make-instance 'list-threads-state)))
        (clue:test "List Threads Message"
            (utils:check-has-result (session::get-next-response state)))))


(defclass kill-thread-state (test-state)
        ())


(defmethod session::get-input-stream ((obj kill-thread-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/killThread\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"id\": 10~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun kill-thread-msg ()
    (let ((state (make-instance 'kill-thread-state))
          (exp (make-hash-table :test #'equalp))
          (err-map (make-hash-table :test #'equalp)))

        (setf (gethash "code" err-map) alive/lsp/errors:*request-failed*)
        (setf (gethash "message" err-map) "Thread 10 not found")

        (setf (gethash "jsonrpc" exp) "2.0")
        (setf (gethash "id" exp) 5)
        (setf (gethash "error" exp) err-map)

        (clue:test "Kill Thread Message"
            (utils:check-equal (session::get-next-response state)
                               exp))))


(defclass list-pkgs-state (test-state)
        ())


(defmethod session::get-input-stream ((obj list-pkgs-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/listPackages\"~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun list-pkgs-msg ()
    (let ((state (make-instance 'list-pkgs-state)))
        (clue:test "List Packages Message"
            (utils:check-has-result (session::get-next-response state)))))


(defclass unexport-state (test-state)
        ())


(defmethod session::get-input-stream ((obj unexport-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/unexportSymbol\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"symbol\": \"foo\",~A" utils:*end-line*)
                       (format str "    \"package\": \"bar\"~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun unexport-symbol-msg ()
    (let ((state (make-instance 'unexport-state)))
        (clue:test "Unexport Symbol Message"
            (utils:check-has-result (session::get-next-response state)))))


(defclass eval-state (test-state)
        ())


(defmethod session::get-input-stream ((obj eval-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/eval\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"package\": \"foo\",~A" utils:*end-line*)
                       (format str "    \"text\": \"(+ 1 2)\"~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defmethod session::send-msg ((obj eval-state) msg)
    (setf (send-called obj) T))


(defun eval-msg ()
    (let ((state (create-state 'eval-state)))
        (clue:test "Eval Message"
            (session::handle-msg state
                                 (session::read-message state))
            (clue:check-equal :expected t
                              :actual (send-called state)))))


(defclass get-pkg-state (test-state)
        ())


(defmethod session::get-input-stream ((obj get-pkg-state))
    (let ((content (with-output-to-string (str)
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
        (utils:stream-from-string (utils:create-msg content))))


(defun get-pkg-msg ()
    (let ((state (create-state 'get-pkg-state)))
        (clue:test "Get Package Message"
            (utils:check-has-result (session::get-next-response state)))))


(defclass remove-pkg-state (test-state)
        ())


(defmethod session::get-input-stream ((obj remove-pkg-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/removePackage\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"name\": \"foo\"~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun remove-pkg-msg ()
    (let ((state (create-state 'remove-pkg-state)))
        (clue:test "Remove Package Message"
            (utils:check-has-result (session::get-next-response state)))))


(defclass list-asdf-state (test-state)
        ())


(defmethod session::get-input-stream ((obj list-asdf-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/listAsdfSystems\"~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defun list-asdf-msg ()
    (let ((state (create-state 'list-asdf-state)))
        (clue:test "List ASDF Systems Message"
            (utils:check-has-result (session::get-next-response state)))))


(defmethod session::get-input-stream ((obj load-asdf-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/loadAsdfSystem\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"name\": \"foo\"~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defmethod session::send-msg ((obj load-asdf-state) msg)
    (setf (send-called obj) T))


(defun load-asdf-msg ()
    (let ((state (create-state 'load-asdf-state)))
        (clue:test "Load ASDF System Message"
            (session::handle-msg state
                                 (session::read-message state))
            (clue:check-equal :expected t
                              :actual (send-called state)))))


(defmethod session::get-input-stream ((obj inspect-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/inspect\",~A" utils:*end-line*)
                       (format str "  \"params\": {~A" utils:*end-line*)
                       (format str "    \"package\": \"cl-user\",~A" utils:*end-line*)
                       (format str "    \"text\": \"(+ 1 2)\"~A" utils:*end-line*)
                       (format str "  }~A" utils:*end-line*)
                       (format str "}~A" utils:*end-line*))))
        (utils:stream-from-string (utils:create-msg content))))


(defmethod session::send-msg ((obj inspect-state) msg)
    (setf (send-called obj) T))


(defun inspect-msg ()
    (let ((state (create-state 'inspect-state)))
        (clue:test "Inspect Message"
            (session::handle-msg state
                                 (session::read-message state))
            (clue:check-equal :expected t
                              :actual (send-called state)))))


(defclass symbol-state (test-state)
        ())


(defmethod session::get-input-stream ((obj symbol-state))
    (let ((content (with-output-to-string (str)
                       (format str "{~A" utils:*end-line*)
                       (format str "  \"jsonrpc\": \"2.0\",~A" utils:*end-line*)
                       (format str "  \"id\": 5,~A" utils:*end-line*)
                       (format str "  \"method\": \"$/alive/symbol\",~A" utils:*end-line*)
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
        (utils:stream-from-string (utils:create-msg content))))


(defun symbol-msg ()
    (let ((state (make-instance 'symbol-state)))
        (clue:test "Symbol Message"
            (utils:check-equal (session::get-next-response state)
                               (list (cons :jsonrpc "2.0")
                                     (cons :id 5)
                                     (cons :result (list (cons :value nil))))))))


(defun run-all ()
    (clue:suite "Session Message Tests"
        (init-msg)
        ;    (load-file-msg)
        (completion-msg)
        (top-form-msg)
        ; (formatting-msg)
        (list-threads-msg)
        (kill-thread-msg)
        (list-pkgs-msg)
        ; (unexport-symbol-msg)
        (get-pkg-msg)
        (remove-pkg-msg)
        (list-asdf-msg)
        (hover-msg)
        (format-on-type-msg)))