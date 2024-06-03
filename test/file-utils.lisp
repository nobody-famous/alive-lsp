(defpackage :alive/test/file-utils
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:utils :alive/file-utils)))

(in-package :alive/test/file-utils)


(defun test-escape-file ()
    (clue:suite "Escape file"
        (clue:test "Unix file"
            (clue:check-equal :expected "a/b/c"
                              :actual (utils:escape-file "a/b/c")))

        (clue:test "Windows file"
            (clue:check-equal :expected "/c%3A/a/b/c"
                              :actual (utils:escape-file "c:/a/b/c")))))


(defun run-all ()
    (clue:suite "File Utils Tests"
        (test-escape-file)))
