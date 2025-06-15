(defpackage :alive/lsp/utils
    (:use :cl)
    (:export :find-tokens
             :fuzzy-match
             :symbol-for-pos)
    (:local-nicknames (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:packages :alive/packages)
                      (:types :alive/types)))

(in-package :alive/lsp/utils)


(defun find-tokens (tokens pos)
    (loop :for token :in tokens

          :collect token :into found-tokens

          :while (pos:less-than (token:get-end token) pos)

          :finally (return (cond ((<= 3 (length found-tokens)) (subseq (reverse found-tokens) 0 3))
                                 ((= 2 (length found-tokens)) (reverse (cons nil found-tokens)))
                                 ((= 1 (length found-tokens)) (list (first found-tokens) nil nil))))))


(defun symbol-for-pos (text pos)
    (let* ((raw-tokens (tokenizer:from-stream (make-string-input-stream text)))
           (tokens (find-tokens raw-tokens pos))
           (pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (or pkg *package*)))

        (unless (zerop (length tokens))
            (destructuring-bind (token1 token2 token3) tokens
                (cond ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*colons*)
                            (eq (token:get-type-value token3) types:*symbol*))
                          (let* ((real-pkg (packages:lookup (token:get-text token3)))
                                 (real-pkg-name (if real-pkg
                                                    (package-name real-pkg)
                                                    (token:get-text token3))))
                              (values (token:get-text token1)
                                  real-pkg-name)))

                      ((eq (token:get-type-value token1) types:*symbol*)
                          (values (token:get-text token1)
                              pkg-name))

                      (T nil))))))
