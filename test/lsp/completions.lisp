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
                  (comps:simple :text "some text alive/symbols::foo  "
                                :pos (pos:create :line 0
                                                 :col 28))

                  (comps:simple :text ":foo"
                                :pos (pos:create :line 0
                                                 :col 1)))))


(defun run-all ()
    (run:suite "Completion Tests"
               (lambda ()
                   (basic))))
