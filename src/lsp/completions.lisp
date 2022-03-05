(defpackage :alive/lsp/completions
    (:use :cl)
    (:export :simple)
    (:local-nicknames (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/lsp/completions)


(defun find-tokens (tokens pos)
    (loop :for token :in tokens

          :collect token :into found-tokens

          :while (pos:less-than (token:end token) pos)

          :finally (return (cond ((<= 3 (length found-tokens)) (subseq (reverse found-tokens) 0 3))
                                 ((= 2 (length found-tokens)) (cons nil (reverse found-tokens)))
                                 ((= 1 (length found-tokens)) (list (first found-tokens) nil nil))))))


(defun simple (&key text pos)
    (let ((tokens (tokenizer:from-stream (make-string-input-stream text))))
        (destructuring-bind (token1 token2 token3) (find-tokens tokens pos)
            (cond ((and (eq (token:get-type-value token1) types:*symbol*)
                        (eq (token:get-type-value token2) types:*colons*)
                        (eq (token:get-type-value token3) types:*symbol*))
                   (format T "SYMBOL WITH PACKAGE~%"))

                  ((and (eq (token:get-type-value token1) types:*colons*)
                        (eq (token:get-type-value token2) types:*symbol*))
                   (format T "PACKAGE WITHOUT NAME~%"))

                  (T (format T "~A ~A ~A~%" token3 token2 token1))))))
