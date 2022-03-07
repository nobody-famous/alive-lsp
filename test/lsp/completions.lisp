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
                  (check:are-equal (list "macro-p")
                                   (comps:simple :text "some text alive/symbols:m"
                                                 :pos (pos:create :line 0
                                                                  :col 27)))

                  (check:are-equal (list "defmethod" "defmacro")
                                   (comps:simple :text "some text cl-user:defm"
                                                 :pos (pos:create :line 0
                                                                  :col 22)))

                  (check:are-equal (list "callable-p" "function-p" "macro-p" "get-lambda-list")
                                   (comps:simple :text "alive/symbols:"
                                                 :pos (pos:create :line 0
                                                                  :col 14)))

                  (check:are-equal (list "*debug-io*")
                                   (comps:simple :text "*debug-io"
                                                 :pos (pos:create :line 0
                                                                  :col 9))))))


(defun run-all ()
    (run:suite "Completion Tests"
               (lambda ()
                   (basic))))
