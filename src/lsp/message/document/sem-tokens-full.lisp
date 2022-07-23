(defpackage :alive/lsp/message/document/sem-tokens-full
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:sem-types :alive/lsp/types/sem-tokens)))

(in-package :alive/lsp/message/document/sem-tokens-full)


(defun to-sem-array (sem-tokens)
    (loop :with line := 0
          :with col := 0
          :with out-list := nil

          :for token :in sem-tokens
          :for len := (- (sem-types:end-col token) (sem-types:start-col token))
          :for line-diff := (- (sem-types:line token) line)
          :for col-diff := (if (zerop line-diff)
                               (- (sem-types:start-col token) col)
                               (sem-types:start-col token)) :do

              (push line-diff out-list)
              (push col-diff out-list)
              (push len out-list)
              (push (sem-types:token-type token) out-list)
              (push 0 out-list)

              (setf line (sem-types:line token))
              (setf col (sem-types:start-col token))
          :finally (return (reverse out-list))))


(defun create-response (id sem-tokens)
    (let ((data (make-hash-table :test #'equalp)))
        (setf (gethash "data" data) (to-sem-array sem-tokens))
        (message:create-response id
                                 :result-value data)))
