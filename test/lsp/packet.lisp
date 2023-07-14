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
            (clue:check-equal :expected (format nil "Content-Length: 2~C~C~C~C10" #\return #\newline #\return #\newline)
                              :actual actual))))


(defun run-all ()
    (clue:suite "Packet tests"
        (test-parse)
        (test-to-wire)))
