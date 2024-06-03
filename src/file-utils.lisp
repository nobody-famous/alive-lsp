(defpackage :alive/file-utils
    (:use :cl)
    (:export :escape-file))

(in-package :alive/file-utils)


(declaim (ftype (function (string) boolean) is-win-file))
(defun is-win-file (file)
    (and file
         (alpha-char-p (char file 0))
         (char= #\: (char file 1))
         (char= #\/ (char file 2))))


(declaim (ftype (function (string) string) escape-win-file))
(defun escape-win-file (file)
    (format nil "~C%3A~A"
        (char file 0)
        (subseq file 2)))


(declaim (ftype (function (string) string) escape-file))
(defun escape-file (file)
    (if (is-win-file file)
        (format nil "/~A" (escape-win-file file))
        file))
