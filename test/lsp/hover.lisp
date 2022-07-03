(defpackage :alive/test/lsp/hover
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:hover :alive/lsp/hover)
                      (:pos :alive/position)

                      (:run :alive/test/harness/run)
                      (:check :alive/test/harness/check)))

(in-package :alive/test/lsp/hover)


(defun basic ()
    (run:test "Basic Hover Tests"
              (lambda ()
                  (check:are-equal T
                                   (stringp (hover:get-text :text "defun"
                                                            :pos (pos:create 0 3)))))))


(defun run-all ()
    (run:suite "Hover Tests"
               (lambda ()
                   (basic))))
