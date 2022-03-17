(defpackage :alive/parse/forms
    (:use :cl)
    (:export :from-stream)
    (:local-nicknames (:form :alive/parse/form)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/parse/forms)


(defun from-stream (input)
    (let ((tokens (tokenizer:from-stream input)))
        (format T "from-stream ~A~%" tokens)))
