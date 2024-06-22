(defpackage :alive/source-utils
    (:use :cl)
    (:export :get-range-from-file
             :get-source-location
             :url-encode-filename)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:loc :alive/location)
                      (:pos :alive/position)))

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


(defun tokens-for-file (file)
    (handler-case
            (with-open-file (in-stream file)
                (alive/parse/tokenizer:from-stream in-stream))
        (T nil)))


(defun forms-for-file (file)
    (handler-case
            (with-open-file (in-stream file)
                (forms:from-stream in-stream))
        (T nil)))


(defun find-form-token (tokens form)
    (loop :while tokens
          :do (let ((top (first tokens)))
                  (if (pos:less-than (gethash "start" top)
                                     (gethash "start" form))
                      (pop tokens)
                      (return tokens)))))


(defun get-source-form (src)
    (let* ((file (sb-introspect:definition-source-pathname src))
           (nth-form (first (sb-introspect:definition-source-form-path src)))
           (form-num (sb-introspect:definition-source-form-number src))
           (file-path (when file (url-encode-filename (namestring file)))))
        (alive/logger:info-msg "***** GET SRC FORM ~A ~A" nth-form form-num)
        (handler-case
                (with-open-file (in-stream file)
                    (let* ((tokens (tokens-for-file file))
                           (forms (forms-for-file file))
                           (form-tokens (find-form-token tokens (nth nth-form forms))))
                        (alive/test/utils:print-hash-table "***** FORM" (first form-tokens))))
            (T nil))))
