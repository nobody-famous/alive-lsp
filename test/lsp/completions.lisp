(defpackage :alive/test/lsp/completions
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:pos :alive/position)))

(in-package :alive/test/lsp/completions)


(defun test-symbols-m ()
    (clue:test "Test symbols:m"
        (let ((expected (list (comps:create-item :label "macro-p"
                                                 :kind 3
                                                 :insert-text "macro-p ${1:sym-name}")))
              (actual (comps:simple :text "some text alive/symbols:m"
                                    :pos (pos:create 0 27))))

            (clue:check-equal :expected (length expected)
                              :actual (length actual)))))


(defun test-defmacro ()
    (clue:test "Test defmacro"
        (let ((expected (list (comps:create-item :label "defmacro"
                                                 :kind 3
                                                 :insert-text "defmacro ${1:name} ${2:lambda-list}")
                              (comps:create-item :label "define-modify-macro"
                                                 :kind 3
                                                 :doc-string "Creates a new read-modify-write macro like PUSH or INCF."
                                                 :insert-text "define-modify-macro ${1:name} ${2:lambda-list} ${3:function}")
                              (comps:create-item :label "define-compiler-macro"
                                                 :kind 3
                                                 :doc-string "Define a compiler-macro for NAME."
                                                 :insert-text "define-compiler-macro ${1:name} ${2:lambda-list}")
                              (comps:create-item :label "define-symbol-macro"
                                                 :kind 3
                                                 :insert-text "define-symbol-macro ${1:name} ${2:expansion}")))
              (actual (comps:simple :text "some text cl-user:defmacro"
                                    :pos (pos:create 0 26))))

            (clue:check-equal :expected (length expected)
                              :actual (length actual)))))


(defun test-symbols ()
    (clue:test "Test symbols"
        (let ((expected (list (comps:create-item :label "external-p"
                                                 :kind 3
                                                 :insert-text "external-p ${1:sym-name}")
                              (comps:create-item :label "macro-p"
                                                 :kind 3
                                                 :insert-text "macro-p ${1:sym-name}")
                              (comps:create-item :label "lookup"
                                                 :kind 3
                                                 :insert-text "lookup ${1:name} ${2:pkg-name}")
                              (comps:create-item :label "callable-p"
                                                 :kind 3
                                                 :insert-text "callable-p ${1:sym-name}")
                              (comps:create-item :label "get-lambda-list"
                                                 :kind 3
                                                 :insert-text "get-lambda-list ${1:fn-name}")
                              (comps:create-item :label "function-p"
                                                 :kind 3
                                                 :insert-text "function-p ${1:name}")))
              (actual (comps:simple :text "alive/symbols:"
                                    :pos (pos:create 0 14))))
            (clue:check-equal :expected (length expected)
                              :actual (length actual)))))


(defun test-debug-io ()
    (clue:test "Test debug-io"
        (let ((expected (list (comps:create-item :label "*invoke-debugger-hook*"
                                                 :kind 1
                                                 :insert-text "*invoke-debugger-hook*")
                              (comps:create-item :label "*debug-condition*"
                                                 :kind 1
                                                 :insert-text "*debug-condition*")
                              (comps:create-item :label "*debug-io*"
                                                 :kind 1
                                                 :insert-text "*debug-io*")))
              (actual (comps:simple :text "*debug-io"
                                    :pos (pos:create 0 9))))
            (clue:check-equal :expected (length expected)
                              :actual (length actual)))))


(defun run-all ()
    (clue:suite "Completion Tests"
        (test-symbols-m)
        (test-defmacro)
        (test-symbols)
        (test-debug-io)))