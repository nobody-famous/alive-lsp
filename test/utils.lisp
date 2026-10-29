(defpackage :alive/test/utils
    (:use :cl)
    (:export :*end-line*
             :create-msg
             :check-equal
             :check-exists
             :check-has-result
             :get-forms
             :get-tokens
             :print-hash-table
             :stream-from-string)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/test/utils)


#+win32
(defparameter *end-line* (format nil "~C" #\newline))

#-win32
(defparameter *end-line* (format nil "~C~C" #\return #\newline))


(defun create-msg (content)
    (with-output-to-string (str)
        (format str "Content-Length: ~A~A" (length (flexi-streams:string-to-octets content)) *end-line*)
        (format str "~A" *end-line*)
        (format str "~A" content)))


(defun stream-from-string (str)
    (flexi-streams:make-flexi-stream
        (flexi-streams:make-in-memory-input-stream
            (flexi-streams:string-to-octets str))))


(defun check-equal (obj1 obj2)
    (unless (equalp obj1 obj2)
        (error "Not equal")))


(defun check-exists (obj)
    (unless obj
        (error "Object is null")))


(defun check-has-result (obj)
    (unless (gethash "result" obj)
        (error "Object has no result")))


(declaim (ftype (function (string (or null hash-table)) null) print-hash-table))
(defun print-hash-table (label ht)
    (when (hash-table-p ht)
          (format T "~A~%" label)
          (loop :for key :being :the :hash-key :of ht
                :using (hash-value value)
                :do (format T "  ~A |~A|~%" key value))))


(defun get-forms (text)
    (let ((tokens (tokenizer:from-stream (make-string-input-stream text))))
        (forms:from-tokens tokens)))


(defun get-tokens (text)
    (tokenizer:from-stream (make-string-input-stream text)))
