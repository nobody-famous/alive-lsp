(defpackage :alive/test/lsp/completions
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:comps :alive/lsp/completions)
                      (:pos :alive/position)
                      (:check :alive/test/harness/check)
                      (:run :alive/test/harness/run)))

(in-package :alive/test/lsp/completions)

(defun basic ()
    (run:test "Basic Completions Test"
              (lambda ()
                  (check:are-equal (list (comps:create-item :label "macro-p"
                                                            :kind 3
                                                            :insert-text "macro-p ${1:sym-name}"))
                                   (comps:simple :text "some text alive/symbols:m"
                                                 :pos (pos:create 0 27)))

                  (check:are-equal (list (comps:create-item :label "defmacro"
                                                            :kind 3
                                                            :insert-text "defmacro ${1:name} ${2:lambda-list}")
                                         (comps:create-item :label "define-modify-macro"
                                                            :kind 3
                                                            :insert-text "define-modify-macro ${1:name} ${2:lambda-list} ${3:function}")
                                         (comps:create-item :label "define-compiler-macro"
                                                            :kind 3
                                                            :insert-text "define-compiler-macro ${1:name} ${2:lambda-list}")
                                         (comps:create-item :label "define-symbol-macro"
                                                            :kind 3
                                                            :insert-text "define-symbol-macro ${1:name} ${2:expansion}"))
                                   (comps:simple :text "some text cl-user:defmacro"
                                                 :pos (pos:create 0 26)))

                  (check:are-equal (list (comps:create-item :label "get-lambda-list"
                                                            :kind 3
                                                            :insert-text "get-lambda-list ${1:fn-name}")
                                         (comps:create-item :label "callable-p"
                                                            :kind 3
                                                            :insert-text "callable-p ${1:sym-name}")
                                         (comps:create-item :label "function-p"
                                                            :kind 3
                                                            :insert-text "function-p ${1:name}")
                                         (comps:create-item :label "macro-p"
                                                            :kind 3
                                                            :insert-text "macro-p ${1:sym-name}")
                                         (comps:create-item :label "lookup"
                                                            :kind 3
                                                            :insert-text "lookup ${1:name} ${2:pkg-name}"))
                                   (comps:simple :text "alive/symbols:"
                                                 :pos (pos:create 0 14)))

                  (check:are-equal (list (comps:create-item :label "*invoke-debugger-hook*"
                                                            :kind 1
                                                            :insert-text "*invoke-debugger-hook*")
                                         (comps:create-item :label "*debug-condition*"
                                                            :kind 1
                                                            :insert-text "*debug-condition*")
                                         (comps:create-item :label "*debug-io*"
                                                            :kind 1
                                                            :insert-text "*debug-io*"))
                                   (comps:simple :text "*debug-io"
                                                 :pos (pos:create 0 9))))))


(defun run-all ()
    (run:suite "Completion Tests"
               (lambda ()
                   (basic))))
