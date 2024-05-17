(defpackage :alive/file-utils
    (:use :cl)
    (:export :escape-file))

(in-package :alive/file-utils)


(defun is-win-file (file)
    (and file
         (alpha-char-p (char file 0))
         (char= #\: (char file 1))
         (char= #\/ (char file 2))))


(defun escape-win-file (file)
    (format nil "~C%3A~A"
        (char file 0)
        (subseq file 2)))


(defun escape-file (file)
    (if (is-win-file file)
        (format nil "/~A" (escape-win-file file))
        file))
