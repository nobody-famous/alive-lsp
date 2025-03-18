(defpackage :alive/test/lsp/sig-help
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:pos :alive/position)
                      (:sig :alive/lsp/sig-help)))

(in-package :alive/test/lsp/sig-help)


(defun check-sig (item label)
    (clue:check-equal :expected T
                      :actual (hash-table-p item))
    (clue:check-equal :expected label
                      :actual (gethash "label" item)))


(defun test-no-signature ()
    (clue:test "No Signature"
        (clue:check-equal :expected nil
                          :actual (sig:signatures :text "123" :pos (pos:create 0 1)))

        (clue:check-equal :expected nil
                          :actual (sig:signatures :text "" :pos (pos:create 0 0)))

        (clue:check-equal :expected nil
                          :actual (sig:signatures :text "(foo x y)" :pos (pos:create 0 3)))

        (clue:check-equal :expected nil
                          :actual (sig:signatures :text "(cl:defun foo () nil)" :pos (pos:create 0 2)))))


(defun test-defun ()
    (clue:test "Defun"
        (check-sig (first (sig:signatures :text "(defun foo () nil)" :pos (pos:create 0 2)))
                   "DEFUN NAME LAMBDA-LIST &BODY BODY")

        (check-sig (first (sig:signatures :text "(cl:defun foo () nil)" :pos (pos:create 0 12)))
                   "DEFUN NAME LAMBDA-LIST &BODY BODY")

        (check-sig (first (sig:signatures :text "(cl:defun foo () nil)" :pos (pos:create 0 16)))
                   "DEFUN NAME LAMBDA-LIST &BODY BODY")))


(defun run-all ()
    (clue:suite "Signature Help"
        (test-no-signature)
        (test-defun)))
