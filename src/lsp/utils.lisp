(defpackage :alive/lsp/utils
    (:use :cl)
    (:export :find-tokens)
    (:local-nicknames (:pos :alive/position)
                      (:token :alive/parse/token)))

(in-package :alive/lsp/utils)


(defun find-tokens (tokens pos)
    (loop :for token :in tokens

          :collect token :into found-tokens

          :while (pos:less-than (token:get-end token) pos)

          :finally (return (cond ((<= 3 (length found-tokens)) (subseq (reverse found-tokens) 0 3))
                                 ((= 2 (length found-tokens)) (reverse (cons nil found-tokens)))
                                 ((= 1 (length found-tokens)) (list (first found-tokens) nil nil))))))
