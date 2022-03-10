(defpackage :alive/format
    (:use :cl)
    (:export :range)
    (:local-nicknames (:pos :alive/position)
                      (:range :alive/range)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/format)


(defun out-of-range (range token)
    (or (pos:less-or-equal (token:end token) (range:start range))
        (pos:less-than (range:end range) (token:start token))))


(defun range (input range)
    (let ((tokens (tokenizer:from-stream input)))

        (loop :for token :in tokens
              :unless (out-of-range range token)
              :do (format T "~A~%" token))))
