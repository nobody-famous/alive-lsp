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


(defparameter not-really-there 5)


(defun test-symbol-doc ()
    (clue:suite "get-symbol-doc"
        (clue:test "Get debug-io"
            (let ((actual (hover::get-symbol-doc "*debug-io*" "cl-user")))
                (clue:check-equal :expected T
                                  :actual (stringp actual))))

        (makunbound 'not-really-there)
        (clue:test "Get nonexistant symbol"
            (let ((actual (hover::get-symbol-doc "not-really-there" "alive/test/lsp/hover")))
                (clue:check-equal :expected T
                                  :actual (stringp actual))))))


(defun run-all ()
    (clue:suite "Hover Tests"
        (test-defun)
        (test-symbol-doc)))
