(defpackage :alive/lsp/sem-analysis
    (:use :cl)
    (:export :to-sem-tokens)
    (:local-nicknames (:pos :alive/parse/pos)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:token :alive/parse/token)
                      (:types :alive/types)))

(in-package :alive/lsp/sem-analysis)


(defclass sem-token ()
    ((token-type :accessor token-type
                 :initform nil
                 :initarg :token-type)
     (line :accessor line
           :initform nil
           :initarg :line)
     (start-col :accessor start-col
                :initform nil
                :initarg :start-col)
     (end-col :accessor end-col
              :initform nil
              :initarg :end-col)))


(defmethod print-object ((obj sem-token) out)
    (format out "{~A line ~A start ~A end ~A}"
            (token-type obj)
            (line obj)
            (start-col obj)
            (end-col obj)))


(defclass analysis-state ()
    ((lex-tokens :accessor lex-tokens
                 :initform nil
                 :initarg :lex-tokens)
     (sem-tokens :accessor sem-tokens
                 :initform nil
                 :initarg :sem-tokens)
     (opens :accessor opens
            :initform nil
            :initarg :opens)))


(defmethod print-object ((obj analysis-state) out)
    (format out "{lex ~A sem ~A opens ~A}"
            (lex-tokens obj)
            (sem-tokens obj)
            (opens obj)))


(defun peek-token (state)
    (car (lex-tokens state)))


(defun next-token (state)
    (let ((token (car (lex-tokens state))))
        (setf (lex-tokens state) (cdr (lex-tokens state)))
        token))


(defun add-sem-token (state start end token-type)
    (loop :for line :from (pos:line start) :to (pos:line end)
          :for start-col := (if (eq line (pos:line start))
                                (pos:col start)
                                0)
          :for end-col := (if (eq line (pos:line end))
                              (pos:col end)
                              #xFFFFFFFF)
          :for new-token := (make-instance 'sem-token
                                           :line line
                                           :start-col start-col
                                           :end-col end-col
                                           :token-type token-type) :do
              (push new-token (sem-tokens state))))


(defun process-next-token (state)
    (let ((token (next-token state)))
        (cond ((eq (token:type-value token) types:*comment*)
               (add-sem-token state
                              (token:start token)
                              (token:end token)
                              sem-types:*comment*))

              ((eq (token:type-value token) types:*string*)
               (add-sem-token state
                              (token:start token)
                              (token:end token)
                              sem-types:*string*))

              ((eq (token:type-value token) types:*ws*) nil)

              (T (error (format nil "Unhandled token: ~A" token))))))


(defun to-sem-tokens (tokens)
    (loop :with state := (make-instance 'analysis-state :lex-tokens tokens)
          :while (lex-tokens state)
          :do (process-next-token state)
          :finally (return (reverse (sem-tokens state)))))
