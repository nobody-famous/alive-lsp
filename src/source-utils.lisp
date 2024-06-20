(defpackage :alive/source-utils
    (:use :cl)
    (:export :get-range-from-file
             :get-source-location
             :url-encode-filename)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:loc :alive/location)))

(in-package :alive/source-utils)


(defun needs-encoding (char)
    (eq char #\:))


(defun encode-char (char)
    (if (needs-encoding char)
        (format nil "%~2,'0X" (char-code char))
        (string char)))


(defun url-encode (str)
    (let ((chars (map 'list (lambda (char)
                                (encode-char char))
                     str)))
        (apply #'concatenate 'string chars)))


(defun url-encode-filename (name)
    (let* ((raw-pieces (uiop:split-string name :separator "/\\"))
           (pieces (mapcar (lambda (piece)
                               (if (string= piece "")
                                   ""
                                   (format NIL "/~A" (url-encode piece))))
                           raw-pieces)))
        (apply #'concatenate 'string pieces)))


(defun get-range-from-file (file source-path)
    (handler-case
            (with-open-file (in-stream file)
                (let ((forms (forms:from-stream in-stream)))
                    (forms:get-range-for-path forms source-path)))
        (T nil)))


(declaim (ftype (function (sb-introspect:definition-source) (or null loc:text-location)) get-source-location))
(defun get-source-location (src)
    (let* ((file (sb-introspect:definition-source-pathname src))
           (form-path (sb-introspect:definition-source-form-path src))
           (file-path (when file (url-encode-filename (namestring file))))
           (range (when (and file form-path) (get-range-from-file file form-path))))
        (when (and file-path range)
              (loc:create file-path range))))
