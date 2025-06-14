(defpackage :alive/lsp/packet
    (:use :cl)
    (:export :content-length
             :create-header
             :packet
             :payload
             :to-wire))

(in-package :alive/lsp/packet)


(defclass header ()
        ((content-length :accessor content-length
                         :initform nil
                         :initarg :content-length)))


#+win32
(defvar end-line (format nil "~C" #\newline))

#-win32
(defvar end-line (format nil "~C~C" #\return #\newline))


(defun string-to-bytes (str)
    (flexi-streams:string-to-octets str :external-format :utf-8))



;;
;; Important safety tip: cannot use built-in commands to convert a number to a string
;; https://github.com/nobody-famous/alive-lsp/issues/79
;;
;; This is only encoding the packet length, so no need to worry about negative numbers, decimals, etc
;;
(defun to-decimal-string (num)
    (loop :with result := ()
          :with char-0 := (char-code #\0)

          :while (< 0 num)
          :do (push (code-char (+ char-0 (mod num 10))) result)
              (setf num (floor (/ num 10)))

          :finally (return (coerce result 'string))))


(defun to-wire (msg)
    (let* ((payload (json:encode-json-to-string msg))
           (bytes (flexi-streams:string-to-octets payload :external-format :utf-8)))
        (concatenate 'vector
            (string-to-bytes "Content-Length: ")
            (string-to-bytes (to-decimal-string (length bytes)))
            #(13 10)
            #(13 10)
            bytes)))


(defun create-header ()
    (make-instance 'header))
