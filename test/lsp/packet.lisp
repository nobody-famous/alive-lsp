(defpackage :alive/test/lsp/packet
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:parse :alive/lsp/parse)
                      (:packet :alive/lsp/packet)))

(in-package :alive/test/lsp/packet)


(defun test-parse ()
    (clue:test "Parse packet"
        (clue:expect-fail (lambda ()
                              (parse::parse-header-line "line"))))

    (clue:test "Add header"
        (let ((header (packet:create-header)))
            (parse::add-to-header header
                                  (list "foo" "bar"))
            (clue:check-equal :expected nil
                              :actual (packet:content-length header)))))


(defun test-to-wire ()
    (clue:test "To wire"
        (let ((actual (packet:to-wire 10)))
            (clue:check-equal :expected #(67 111 110 116 101 110 116 45 76 101 110 103 116 104 58 32 50 13 10 13 10 49 48)
                              :actual actual)))

    (clue:test "UTF-8"
        (let* ((ht (make-hash-table :test #'equalp)))
            (setf (gethash "foo" ht) "る")
            (clue:check-equal :expected #(67 111 110 116 101 110 116 45 76 101 110 103 116 104 58 32 49 54 13 10 13 10 123 34 102 111 111 34 58 34 92 117 51 48 56 66 34 125)
                              :actual (packet:to-wire ht)))))


(defun run-all ()
    (clue:suite "Packet tests"
        (test-parse)
        (test-to-wire)))
