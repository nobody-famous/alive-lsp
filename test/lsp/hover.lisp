(defpackage :alive/test/lsp/hover
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:hover :alive/lsp/hover)
                      (:pos :alive/position)))

(in-package :alive/test/lsp/hover)


(defun test-defun ()
    (clue:test "Defun hover"
        (clue:check-equal :expected T
                          :actual (stringp (hover:get-text :text "defun"
                                                           :pos (pos:create 0 3))))))


(defun test-symbol-doc ()
    (clue:test "Get symbol doc"
        (let ((actual (hover::get-symbol-doc "*debug-io*" "cl-user")))
            (clue:check-equal :expected T
                              :actual (stringp actual)))))


(defun run-all ()
    (clue:suite "Hover Tests"
        (test-defun)
        (test-symbol-doc)))
