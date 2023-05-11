(defpackage :alive/test/coverage
    (:use :cl)
    (:export :run))

(in-package :alive/test/coverage)

(defun run ()
    (progn (declaim (optimize (sb-cover:store-coverage-data 3)))

           (asdf:oos 'asdf:load-op :alive-lsp :force t)

           (alive/test/suite:run-all)

           (sb-cover:report "coverage/")

           (declaim (optimize (sb-cover:store-coverage-data 0)))))
